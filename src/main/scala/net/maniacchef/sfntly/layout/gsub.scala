package net.maniacchef.sfntly.layout

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.BufferedInputStream

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import com.google.typography.font.sfntly.FontFactory
import com.google.typography.font.sfntly.Font
import com.google.typography.font.sfntly.Tag
import com.google.typography.font.sfntly.data.WritableFontData
import com.google.typography.font.sfntly.table.Header
import com.google.typography.font.sfntly.table.Table
import com.google.typography.font.sfntly.table.core.CMap
import com.google.typography.font.sfntly.table.core.CMapTable

// Subtable base interface for GSUB/GPOS

abstract class SubTable(val tableType:Int)

// Layout common tables

case class LangSysTable(
  langSysTag:String,
  featureList:List[Int]
)

case class ScriptTable(
  scriptTag:String,
  defaultLangSys:LangSysTable, // DFLT
  langSysList:List[LangSysTable]
)

case class FeatureTable(
  featureTag:String,
  lookupList:List[Int]
)

case class LookupTable(
  lookupType:Int,
  subtableList:List[SubTable]
) {
  val lookupFlag = 0 // Fixed for now
}

abstract class CoverageTable(val format:Int)

case class CoverageTableFormat1(glyphArray:List[Int]) extends CoverageTable(1)

// Subtable type 1

abstract class SingleSubst(val format:Int) extends SubTable(1)

case class SingleSubstFormat2(
  coverage:CoverageTable,
  substitutes:List[Int]
) extends SingleSubst(2)

// Subtable type 2

abstract class MultipleSubst(val format:Int) extends SubTable(2)

case class MultipleSubstFormat1(
  coverage:CoverageTable,
  sequences:List[List[Int]]
) extends MultipleSubst(1)

// Subtable type 4 TODO(bashi): Implement
  
// Subtable type 6

case class SubstLookup(sequenceIndex:Int, lookupIndex:Int)

abstract class ChainingContext(val format:Int) extends SubTable(6)

case class ChainingContextFormat3(
  backtrackList:List[CoverageTable],
  inputList:List[CoverageTable],
  lookaheadList:List[CoverageTable],
  substLookupList:List[SubstLookup]
) extends ChainingContext(3)

// GSUB table

case class GsubTable(
  scripts:List[ScriptTable],
  features:List[FeatureTable],
  lookups:List[LookupTable]
)

// Builder

class GsubTableBuilder(val font:Font) {
  var lookups = ArrayBuffer[LookupTable]()

  var lookupIndexMap = Map[(List[Int],List[Int]), Int]()

  var featureListMap = Map[String, ListBuffer[Int]]()

  // Assumes the font has UCS-4 or UCS-2 cmap
  private def getCMap():CMap = {
    val cmapTable:CMapTable = font.getTable(Tag.cmap)
    (cmapTable.cmap(Font.PlatformId.Windows.value(), Font.WindowsEncodingId.UnicodeUCS4.value()),
     cmapTable.cmap(Font.PlatformId.Windows.value(), Font.WindowsEncodingId.UnicodeUCS2.value()))
    match {
      case (ucs4, ucs2) if (ucs4 != null) => ucs4
      case (_, ucs2) => ucs2
    }
  }

  private def glyphId(ch:Char) = getCMap().glyphId(ch.toInt)

  // Returns the index of appended lookup
  private def appendLookup(ltype:Int, subtableList:List[SubTable]) = {
    lookups += LookupTable(ltype, subtableList)
    lookups.length - 1
  }

  private def singleSubst(g:Int, r:Int) = {
    val key = ((List(g), List(r)))
    if (!lookupIndexMap.contains(key)) {
      val coverage = CoverageTableFormat1(List(g))
      val substitutes = List(r)
      val singleSubst = SingleSubstFormat2(coverage, substitutes)
      val lookupIndex = appendLookup(1, List(singleSubst))
      lookupIndexMap += (key -> lookupIndex)
    }
    lookupIndexMap(key)
  }

  private def multipleSubst(g:Int, rx:List[Int]) = {
    val key = ((List(g), rx))
    if (!lookupIndexMap.contains(key)) {
      val coverage = CoverageTableFormat1(List(g))
      val multipleSubst = MultipleSubstFormat1(coverage, List(rx))
      val lookupIndex = appendLookup(2, List(multipleSubst))
      lookupIndexMap += (key -> lookupIndex)
    }
    lookupIndexMap(key)
  }

  private def ligSubst(gx:List[Int], r:Int) = {
    // TODO(bashi):Implement
    0
  }

  private def createSubstLookupIndexes(gs:List[Int], rs:List[Int]):List[Int] = (gs, rs) match {
    case (x :: List(), y :: List()) => singleSubst(x, y) :: List()
    case (x :: xss, y :: List()) => ligSubst(gs, y) :: List()
    case (x :: List(), y :: yss) => multipleSubst(x, rs) :: List()
    case (x :: xss, y :: yss) => singleSubst(x, y) :: createSubstLookupIndexes(xss, yss)
    case (List(), List()) => List()
  }

  def calt(input:String, subst:String) {
    val gs = input.map(glyphId).toList
    val rs = subst.map(glyphId).toList

    val substLookups = createSubstLookupIndexes(gs, rs).zipWithIndex.map {
      case (l, i) => SubstLookup(i, l)
    }
    val inputCoverages = gs.map((g:Int) => CoverageTableFormat1(List(g)))
    val spaceCoverage = CoverageTableFormat1(List(glyphId(' ')))
    val backtrackCoverages = List(spaceCoverage)
    val lookaheadCoverages = List(spaceCoverage)
    val chainingContext = ChainingContextFormat3(backtrackCoverages, inputCoverages,
                                                 lookaheadCoverages, substLookups)
    val lookupIndex = appendLookup(6, List(chainingContext))
    if (!featureListMap.contains("calt"))
      featureListMap += ("calt" -> ListBuffer[Int]())
    featureListMap("calt") += lookupIndex
  }

  // Build table
  def build():GsubTable = {
    val features = featureListMap.toList.map {
      case (tag, lookups) => FeatureTable(tag, lookups.toList)
    }
    // Multiple script isn't supported. Install 'latn'.
    val langSys = LangSysTable("DFLT", List.range(0, features.length))
    val script = ScriptTable("latn", langSys, List())
    var table = new GsubTable(List(script), features, lookups.toList)
    table
  }
}

// Serializer

class GsubTableSerializer(t:GsubTable) {
  var gsub = t
  var data = WritableFontData.createWritableFontData(0) // growable
  var pos:Int = 0

  def serialize() = {
    pos = 0
    advance(data.writeULong(pos, 0x00010000)) // Version
    reserve(6) // For offsets

    val scriptOffset = scriptList()
    val featureOffset = featureList()
    val lookupOffset = lookupList()

    data.writeUShort(4, scriptOffset)
    data.writeUShort(6, featureOffset)
    data.writeUShort(8, lookupOffset)
    data
  }

  def advance(writer: => Int) { pos += writer }
  def reserve(len:Int) = { val ret = pos; pos += len; ret }
  def reserveForOffset() = { reserve(2) }

  def scriptList() = {
    val startOffset = pos

    advance(data.writeUShort(pos, gsub.scripts.length))
    // ScriptRecords
    val positions = gsub.scripts.map {
      s => {
        advance(data.writeULong(pos, Tag.intValue(s.scriptTag)))
        reserveForOffset()
      }
    }
    // ScriptTables
    val offsets = gsub.scripts.map(scriptTable)
    // Fill ScriptTable offsets
    positions.zip(offsets).foreach {
      case (p, o) => data.writeUShort(p, o - startOffset)
    }
    startOffset
  }

  def scriptTable(s:ScriptTable) = {
    val startOffset = pos

    val defaultLangSysPosition = reserveForOffset()
    advance(data.writeUShort(pos, s.langSysList.length))
    // LangSysRecords
    val positions = s.langSysList.map { l => {
      advance(data.writeULong(pos, Tag.intValue(l.langSysTag)))
      reserveForOffset()
    } }
    // DefaultLangSys
    val defaultLangSysOffset =
      if (s.defaultLangSys != null) langSysTable(s.defaultLangSys) - startOffset
      else 0
    data.writeUShort(defaultLangSysPosition, defaultLangSysOffset)
    // LangSysTables
    val offsets = s.langSysList.map(langSysTable)
    // Fill LangSysTable offsets
    positions.zip(offsets).foreach {
      case (p, o) => data.writeUShort(p, o - startOffset)
    }
    startOffset
  }

  def langSysTable(l:LangSysTable) = {
    val startOffset = pos
    advance(data.writeUShort(pos, 0)) // LookupOrder
    advance(data.writeUShort(pos, 0xffff)) // ReqFeatureIndex
    advance(data.writeUShort(pos, l.featureList.length))
    l.featureList.foreach { i => advance(data.writeUShort(pos, i)) }
    startOffset
  }

  def featureList() = {
    val startOffset = pos

    advance(data.writeUShort(pos, gsub.features.length))
    // FeatureRecords
    val positions = gsub.features.map { f => {
      advance(data.writeULong(pos, Tag.intValue(f.featureTag)))
      reserveForOffset()
    } }
    // FeatureTables
    val offsets = gsub.features.map(featureTable)
    positions.zip(offsets).foreach {
      case (p, o) => data.writeUShort(p, o - startOffset)
    }
    startOffset
  }

  def featureTable(f:FeatureTable) = {
    val startOffset = pos
    advance(data.writeUShort(pos, 0)) // FeatureParams
    advance(data.writeUShort(pos, f.lookupList.length))
    f.lookupList.foreach { i => advance(data.writeUShort(pos, i)) }
    startOffset
  }

  def lookupList() = {
    val startOffset = pos

    val count = gsub.lookups.length
    advance(data.writeUShort(pos, count))
    val offsetsStart = reserve(2 * count) // For Lookup offsets
    val offsets = gsub.lookups.map(lookupTable)
    offsets.zipWithIndex.foreach {
      case (o, i) => {
        data.writeUShort(2 * i + offsetsStart, o - startOffset)
      }
    }
    startOffset
  }

  def lookupTable(l:LookupTable) = {
    val startOffset = pos

    advance(data.writeUShort(pos, l.lookupType))
    advance(data.writeUShort(pos, l.lookupFlag))
    advance(data.writeUShort(pos, l.subtableList.length))
    val offsetsStart = reserve(2 * l.subtableList.length)
    val offsets = l.subtableList.map { s => s match {
      case single2:SingleSubstFormat2 => singleSubstFormat2(single2)
      case multiple1:MultipleSubstFormat1 => multipleSubstFormat1(multiple1)
      case chaining3:ChainingContextFormat3 => chainingContextFormat3(chaining3)
      case _ => { println("Ugh"); 0 }
    } }
    offsets.zipWithIndex.foreach {
      case (o, i) => data.writeUShort(2 * i + offsetsStart, o - startOffset)
    }
    startOffset
  }

  def coverage(c:CoverageTable) = {
    val startOffset = pos

    c match {
      case c1:CoverageTableFormat1 => {
        advance(data.writeUShort(pos, c1.format))
        advance(data.writeUShort(pos, c1.glyphArray.length))
        c1.glyphArray.foreach { g => advance(data.writeUShort(pos, g)) }
      }
      case _ => Unit
    }

    startOffset
  }

  def singleSubstFormat2(s:SingleSubstFormat2) = {
    val startOffset = pos
    advance(data.writeUShort(pos, s.format))
    val coveragePosition = reserveForOffset()
    advance(data.writeUShort(pos, s.substitutes.length))
    s.substitutes.foreach { g => advance(data.writeUShort(pos, g)) }
    val coverageOffset = coverage(s.coverage) - startOffset
    data.writeUShort(coveragePosition, coverageOffset)
    startOffset
  }

  def sequenceTable(gs:List[Int]) = {
    val startOffset = pos
    advance(data.writeUShort(pos, gs.length))
    gs.foreach { g => advance(data.writeUShort(pos, g)) }
    startOffset
  }

  def multipleSubstFormat1(m:MultipleSubstFormat1) = {
    val startOffset = pos
    advance(data.writeUShort(pos, m.format))
    val coveragePosition = reserveForOffset()
    advance(data.writeUShort(pos, m.sequences.length))
    val offsetsStart = reserve(2 * m.sequences.length)
    val coverageOffset = coverage(m.coverage) - startOffset
    data.writeUShort(coveragePosition, coverageOffset)
    // SequenceTables
    var offsets = m.sequences.map(sequenceTable)
    offsets.zipWithIndex.foreach {
      case (o, i) => data.writeUShort(2 * i + offsetsStart, o - startOffset)
    }
    startOffset
  }

  def chainingContextFormat3(c:ChainingContextFormat3) = {
    val startOffset = pos

    advance(data.writeUShort(pos, c.format))
    advance(data.writeUShort(pos, c.backtrackList.length))
    val backtrackPosition = reserve(2 * c.backtrackList.length)
    advance(data.writeUShort(pos, c.inputList.length))
    val inputPosition = reserve(2 * c.inputList.length)
    advance(data.writeUShort(pos, c.lookaheadList.length))
    val lookaheadPosition = reserve(2 * c.lookaheadList.length)

    // SubstLookupRecords
    advance(data.writeUShort(pos, c.substLookupList.length))
    c.substLookupList.foreach { s => {
      advance(data.writeUShort(pos, s.sequenceIndex))
      advance(data.writeUShort(pos, s.lookupIndex))
    } }

    // Create coverage tables and fill offsets
    val backtrackOffsets = c.backtrackList.map(coverage)
    backtrackOffsets.zipWithIndex.foreach {
      case (o, i) => data.writeUShort(2 * i + backtrackPosition, o - startOffset)
    }
    val inputOffsets = c.inputList.map(coverage)
    inputOffsets.zipWithIndex.foreach {
      case (o, i) => data.writeUShort(2 * i + inputPosition, o - startOffset)
    }
    val lookaheadOffsets = c.lookaheadList.map(coverage)
    lookaheadOffsets.zipWithIndex.foreach {
      case (o, i) => data.writeUShort(2 * i + lookaheadPosition, o - startOffset)
    }

    startOffset
  }
}

// APIs

class Gsub(n:String, f:Font) {
  private val srcFile = n
  private val font = f
  private var builder = new GsubTableBuilder(f)

  private def getFontBuilder():Font.Builder = {
    val binaryStream = new BufferedInputStream(new FileInputStream(srcFile))
    val fontBuilders = FontFactory.getInstance().loadFontsForBuilding(binaryStream)
    binaryStream.close()
    fontBuilders(0)
  }

  def contextualAlternate(rules: => Any):Gsub = {
    rules match {
      case (a:String, b:String) => builder.calt(a, b)
      case x :: xs => { contextualAlternate(x); contextualAlternate(xs) }
      case _ => Unit
    }
    this
  }

  def done(outFile:String) {
    var serializer = new GsubTableSerializer(builder.build())
    val newGsubData = serializer.serialize()
    val fontBuilder = getFontBuilder()
    val gsubBuilder = fontBuilder.newTableBuilder(Tag.GSUB, newGsubData)
    gsubBuilder.data().setCheckSumRanges()
    val outFont = fontBuilder.build()
    val fileStream = new FileOutputStream(outFile)
    FontFactory.getInstance().serializeFont(outFont, fileStream)
    fileStream.close()
  }
}

object Gsub {
  private def loadFont(fontFile:String):Font = {
    val binaryStream = new BufferedInputStream(new FileInputStream(fontFile))
    val fonts = FontFactory.getInstance().loadFonts(binaryStream)
    binaryStream.close()
    fonts(0)
  }
  
  def apply(srcFile:String) = {
    new Gsub(srcFile, loadFont(srcFile))
  }
}
