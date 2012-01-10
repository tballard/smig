/*
 * srcbin
 */

package smig 
  
import net.miginfocom.swing.MigLayout
import net.miginfocom.layout.ComponentWrapper
import net.miginfocom.layout.ConstraintParser
import net.miginfocom.layout.LayoutCallback
import net.miginfocom.layout.LayoutUtil
import net.miginfocom.layout.UnitValue
import java.awt.Color
import java.awt.Dimension
import java.awt.event.ContainerEvent
import java.awt.event.ContainerListener
import javax.swing.JPanel
import javax.swing.SwingUtilities
import javax.swing.ToolTipManager
import net.miginfocom.layout.BoundSize
import scala.collection.JavaConversions._
import scala.swing.Component
import scala.swing.LayoutContainer
import scala.swing.Panel

/**
 * First things first.  A smig is a kind of beard.  Sorta the goatee motif.
 * 
 * This is a wrapper for the mig layout manager for use from Scala.
 * This was my first Scala project, so it probably has some lame points.
 * It was designed to make things more type-safe than the java version
 * because, although not personally a nazi, I prefer APIs that are.
 * 
 * Ready for production use?  Not exactly, but heck it's just one file.  Pitch
 * it in your project, patch it if it needs it and tell me about it.
 * 
 * It requires the mig jar file.  I was compiling against miglayout-4.0.jar.
 * Depending on the platform you need miglayout-4.0-swing.jar or similar.
 * 
 * I picked and chose features to a slight degree when there seemed to be 
 * duplicated capability that was confusing or poorly documented.  Feel free,
 * nay, obliged to educate me.
 * 
 * I generally stuck with chaining instead of properties since it seemed more
 * succinct and the style is much like java which may be familiar to mig users.
 * 
 * tom@geeksite.org
 */
  
/** Specify 4 possible dock positions in component constraint */
object Dock extends Enumeration {
  val North, West, South, East = Value
  
  private [smig] implicit def toString(dock: Dock.Value) = 
    dock.toString.toLowerCase
}

/** No constructor for you, but an XPos. Float, PCT, Int works */
class AlignX private[smig](a: String) {
  private[smig] val _a = a
}
object AlignX {
  implicit def toAlignX(p: XPos.Value) =
    new AlignX(p.toString.substring(1))
  implicit def toAlignX(f: Float) = new AlignX(f.toString)
  implicit def toAlignX(p: PCT) = new AlignX(p._value.getValue + "%")
  implicit def toAlignX(uv: UV) = new AlignX(uv.toString)
  implicit def toAlignX(uv : UnitValue) = 
    new AlignX(uv.getValue + UV.typeString(uv.getUnit))
  
  lazy val ZERO = AlignX.toAlignX(PX(0))
  lazy val CENTER = AlignX.toAlignX(PCT(50))
  lazy val LEFT = AlignX.toAlignX(PCT(0))
  lazy val RIGHT = AlignX.toAlignX(PCT(100))
  lazy val LEADING = AlignX.toAlignX(PCT(0))
  lazy val TRAILING = AlignX.toAlignX(PCT(100))
}

class AlignY private[smig](a: String) {
  private[smig] val _a = a
}
object AlignY  {
  implicit def toAlignY(p: YPos.Value) =
    new AlignY(p.toString.substring(1))
  implicit def toAlignY(f: Float) = new AlignY(f.toString)
  implicit def toAlignY(p: PCT) = new AlignY(p._value.getValue + "%")
  implicit def toAlignY(uv: UV) = new AlignY(uv.toString)
  
  lazy val ZERO = AlignY.toAlignY(PX(0))
  lazy val CENTER = AlignY.toAlignY(PCT(50))
  lazy val TOP = AlignY.toAlignY(PCT(0))
  lazy val BOTTOM = AlignY.toAlignY(PCT(100))
  lazy val LEADING = AlignY.toAlignY(PCT(0))
  lazy val TRAILING = AlignY.toAlignY(PCT(100))
}

/** These are specialized X alignments. Substring(1) of the name is the
 * string MigLayout uses 
 */
object XPos extends Enumeration {
  type XPos = Value
  val Xleft, Xright, Xcenter, Xleading, Xtrailing, Xbaseline = Value
}

/** These are specialized Y alignments */
object YPos extends Enumeration {
  type YPos = Value
  val Ytop, Ybottom, Ycenter, Yleading, Ytrailing, Ybaseline = Value
}
  
object HideMode extends Enumeration {
  type HideMode = Value
  val Visible, ZeroSquareGaps,  ZeroSquareNoGaps, NoCell = Value
}
  
/** 
 * Privately constructed base class for unit values.  Use the derived 
 * classes for strong typing.  Example: PX(2) 
 */
class UV private[smig] (uv: UnitValue) {
  protected[smig] var _value : UnitValue = uv
  private[smig] def this() = this(null)
    
  override def toString : String = {
    _value.getValue + UV.typeString(_value.getUnit)
  }
}
object UV {
  
  implicit def toUV(f: Float) : UV = {
    PX(f)
  }
  implicit def toUV(i: Int) : UV = {
    PX(i.floatValue)
  }
  implicit private[smig] def toUV(unitValue: UnitValue) : UV = {
    if (unitValue == null) N else {
      val value: Float = unitValue.getValue
      val unit: Int = unitValue.getUnit
      unit match {
        case UnitValue.ALIGN => AL(value)
        case UnitValue.LPX => LPX(value)
        case UnitValue.LPY => LPY(value)
        case UnitValue.PERCENT => PCT(value)
        case UnitValue.PIXEL => PX(value)
        case UnitValue.SPX => SPX(value)
        case UnitValue.SPY => SPY(value)
        case UnitValue.INCH => IN(value) 
        case UnitValue.MM => MM(value)
        case UnitValue.CM => CM(value)
        case _ => new UV(unitValue)
      }
    }
  }
  private[smig] def typeString(i: Int) : String = {
    i match {
      case UnitValue.PIXEL => "px"
      case UnitValue.LPX => "lpx"
      case UnitValue.LPY => "lpy"
      case UnitValue.PERCENT => "%"
      case UnitValue.CM => "cm"
      case UnitValue.INCH => "in"
      case UnitValue.SPX => "spx"
      case UnitValue.SPY => "spy"
      case UnitValue.ALIGN => "al"
      case UnitValue.MM => "mm"
      case UnitValue.PT => "pt"
      case UnitValue.MIN_SIZE => "min"
      case UnitValue.PREF_SIZE => "pref"
      case UnitValue.MAX_SIZE => "max"
      case UnitValue.BUTTON => "button"
      case UnitValue.LABEL_ALIGN => "label"
      case _ => "?"
    }
  }
}

/** UV for align. strongly typed AL(25) becomes "25.0al" */
final class AL private (f: Float) extends UV { 
  _value = new UnitValue(f, UnitValue.ALIGN, f + "al")
}
object AL {
  def apply(a: Float) = new AL(a)
}

/** Specialize UV for logical pixels so spaces try to grow with fonts. */
final class LPX private (f: Float) extends UV {
  _value = new UnitValue(f, UnitValue.LPX, f + "lp")
}
object LPX {
  def apply(f: Float) = new LPX(f)
}

/** Specialize UV for logical pixels so spaces try to grow with fonts. */
final class LPY private (f: Float) extends UV {
  _value = new UnitValue(f, UnitValue.LPY, f + "lp")
}
object LPY {
  def apply(f: Float) = new LPY(f)
}

/** UV for percent. strongly typed PCT(25) becomes "25.0%" */
final class PCT private (percent: Float) extends UV { 
  _value = new UnitValue(percent, UnitValue.PERCENT, percent + "%")
}
object PCT {
  def apply(percent: Float) = new PCT(percent)
}

/** Specialize UV for pixels */
final class PX private (f: Float) extends UV {
  _value = new UnitValue(f, UnitValue.PIXEL, f + "px")
} 
object PX {
  def apply(f: Float) : PX = new PX(f)
}
  
/** Specialize UV for, you know, Screen Percentatge */
final class SPX private (f: Float) extends UV {
  _value = new UnitValue(f, UnitValue.SPX, f + "spx")
}
object SPX {
  def apply(f: Float) : SPX = new SPX(f)
}
  
/** Specialize UV for, you know, Screen Percentatge */
final class SPY private (f: Float) extends UV {
  _value = new UnitValue(f, UnitValue.SPY, f + "spy")
}
object SPY {
  def apply(f: Float) : SPY = new SPY(f)
}
  
/** Extend UV for inputting inches */
final class IN private (f: Float) extends UV {
  _value = new UnitValue(f, UnitValue.INCH, f + "in")
}
object IN {
  def apply(f: Float) = new IN(f)
}
  
/** Extend UV for inputting millimeters */
final class MM private (f: Float) extends UV {
  _value = new UnitValue(f, UnitValue.MM, f + "mm")
}
object MM {
  def apply(f: Float) = new MM(f)
}
  
/** Extend UV for inputting centimeters */
final class CM private (f: Float) extends UV {
  _value = new UnitValue(f, UnitValue.CM, f + "cm")
}
object CM {
  def apply(f: Float) = new CM(f)
}
  
/** FS means freestyle.  Allow inputting any old crap without any checking. */
final class FS private (s: String) extends UV {
  _value = ConstraintParser.parseUnitValue(s, true)
}
object FS {
  def apply(s: String) = new FS(s)
}
  
/** Extend UV for null field in BS */
final object N extends UV { 
  _value = null
  
  override def toString = "n"
}

/** 
 *   BBBBB   SSSS
 *   B    B S
 *   BBBBB   SSSS  ************************************************************
 *   B    B      S
 *   BBBBBB SSSSS
 *   
 * Implements the Mig BoundSize */
class BS(_min : UV, _pref: UV, _max: UV) {
  private[this] var v_min: UV = if (_min == null) N else _min
  private[this] var v_pref: UV = if (_pref == null) N else _pref
  private[this] var v_max: UV = if (_max == null) N else _max
  
  /** All nulls */  
  def this() = this(N, N, N)
    
  /** Contains preferred value only. */
  def this(pref: UV) = this(N, pref, N)
    
  /** Contains minimum and preferred values only. */
  def this(min: UV, pref: UV) = this(min, pref, N)
    
  def getMin = _min
  def getPref = _pref
  def getMax = _max
    
  def min(uv: UV) : this.type = { v_min = uv; this }
  def pref(uv: UV) : this.type = { v_pref = uv; this }
  def max(uv: UV) : this.type = { v_max = uv ; this }
    
  /** @return The string that could be used in java */
  override def toString() : String = {
    new StringBuilder().append(_min).append(':').append(_pref).
    append(':').append(_max).toString
  }
}

/* Conversions, creation methods for Bound Size. */
object BS {
  def apply() : BS = new BS()
  def apply(pref: UV) : BS = new BS(pref)
  def apply(min: UV, pref: UV) : BS = new BS(min, pref)
  def apply(min: UV, pref: UV, max: UV) : BS = new BS(min, pref, max)
    
  /** Convert an Int to a BS */
  implicit def toBS(i: Int) : BS = {
    BS(UV.toUV(i))
  }
    
  /** Convert a UV to a BS */
  implicit def toBS(uv: UV) : BS = BS(uv)
    
  /** Convert a java BoundSize to a scala BS */
  implicit def toBS(boundSize: BoundSize) : BS = {
    BS(
      UV.toUV(boundSize.getMin),
      UV.toUV(boundSize.getPreferred),
      UV.toUV(boundSize.getMax))
  }
}

/** 
 *   L       CCCC
 *   L      C
 *   L      C      ***********************************************************
 *   L      C
 *   LLLLLL  CCCCC
 *   
 * Layout constraints */
class LC private[smig] (lc: net.miginfocom.layout.LC) {
  private val _lc = lc
    
  /**
   * The grid will wrap to a new column/row after a certain number of columns 
   * (for horizontal flow) or rows (for vertical flow).
   */
  def getWrapAfter = _lc.getWrapAfter
   
  /** 
   * Explicitly specify where to wrap.  I need to be convinced of the 
   * value of wrap() before implementing it.
   */
  def wrapAfter(i: Int) : this.type = {
    require(i >= 0)
    _lc.wrapAfter(i)
    this
  }
  
  /** An XPos. Float, PCT or UV will be converted */
  def alignX(align: AlignX) : this.type = {
    require(align != null)
    _lc.alignX(align._a)
    this
  }
  
  /** A YPos. Float, PCT or UV will be converted */
  def alignY(align: AlignY) : this.type = {
    require(align != null)
    _lc.alignY(align._a)
    this
  }
    
  def getAlignX : Option[AlignX] = {
    val unitValue = _lc.getAlignX
    if (unitValue == null) None else 
      Some(AlignX.toAlignX(unitValue.getValue))
  }
        
  def getAlignY : Option[AlignY] = {
    val unitValue = _lc.getAlignY
    if (unitValue == null) None else
      Some(AlignY.toAlignY(unitValue.getValue))
  }

  /** Glory in the convenience. */
  def align(ax: AlignX, ay: AlignY) : this.type = {
    alignX(ax)
    alignY(ay)
  }
  
  /** 
   * Specifies if the components should be added in the grid bottom-to-top
   * or top-to-bottom. This value is not picked up from the container and is
   * top-to-bottom by default.
   */
  def addBottomToTop(upFlag: Boolean) : this.type = {
    if (upFlag) _lc.bottomToTop else _lc.topToBottom; 
    this 
  }
  
  /** 
   * Overrides the container's ComponentOrientation property for this layout.
   * Normally this value is dependant on the Locale that the application is
   * running. This constraint overrides that value.
   */
  def leftToRight(l: Boolean) : this.type = { _lc.leftToRight(l); this }
    
  /**
   * Get value set by debug(millis: Int)
   */
  def getDebug = _lc.getDebugMillis
    
  /**
   * Turns on debug painting for the container. This will lead to an active 
   * repaint every millis milliseconds. Default value is 1000 (once every
   * second). 
   */
  def debug(millis: Int) : this.type = {
    require(millis >= 0)
    _lc.debug(millis)
    this
  }
  def debug() : this.type = debug(1000)
  
  /**
   * Puts the layout in horizontal flow mode. This means that the next cell
   * is normally below and the next component will be put there instead of
   * to the right. Default is horizontal flow.
   */
  def flowX : this.type = { _lc.flowX; this }
  
  /**
   * Puts the layout in vertical flow mode. This means that the next cell
   * is normally below and the next component will be put there instead of
   * to the right. Default is horizontal flow.
   */
  def flowY : this.type = { _lc.flowY; this }
  
  def isFlowX = _lc.isFlowX
    
  /** 
   * Claims all available space in the container for the columns and/or
   * rows. At least one component need to have a "grow" constaint for it to
   * fill the container. The space will be divided equally, though honoring
   * "growpriority". If no column/row has "grow" set the grow weight of
   * the components in the rows/columns will migrate to that row/column.
   */
  def fillX : this.type = { _lc.fillX; this }
  def isFillX = _lc.isFillX
  def fillY : this.type = { _lc.fillY; this }
  def isFillY = _lc.isFillY
  def fill : this.type = { fillX; fillY; this }
    
  def getHeight : BS = BS.toBS(_lc.getHeight)
    
  /* 
   * The height for the container as a BS. The value will override any value
   * that is set on the container itself.
   */
  def height(h: BS) : this.type = { _lc.height(h.toString); this }
  
  /** 
   * Specified the insets for the laid out container. The gaps before/after
   * the first/last column/row overrides these layout insets. This is the
   * same thing as setting an EmptyBorder on the container but without
   * removing any border already there. Default value is "panel" (or zero
   * if there are docking components). The size of "dialog" and "panel"
   * insets is returned by the current PlatformConverter. The inset values
   * all around can also be set explicitly for one or more sides. Insets on
   * sides that are set to N will get the default which is the platform
   * default for "panel".
   */
  def insets(top: Option[UV], left: Option[UV], bottom: Option[UV],
             right: Option[UV]) : this.type = {
    val oldInsets : Array[UnitValue] = _lc.getInsets
    if (oldInsets == null) {
      def str(uv: Option[UV]) : String = 
      { uv match { case None => null; case Some(x) => x.toString } }
      _lc.insets(str(top), str(left), str(bottom), str(right))
    } else {
      def better(o: UnitValue, uv: Option[UV]) : String =
      { (uv match { case None => N; case _ => uv }).toString }
      _lc.insets(better(oldInsets(0), top), better(oldInsets(1), left),
                 better(oldInsets(2), bottom), better(oldInsets(3), right));
    }
    this
  }
  def insets(inset: UV) : this.type = {
    val i : Option[UV] = Some(inset)
    insets(i, i, i, i)
  }
  /** Use platform defaults for dialog */
  def insetsForDialog() = 
    _lc.setInsets(ConstraintParser.parseInsets("dialog", true));
  /** Use platform defaults for panel.  The default */
  def insetsForPanel() = 
    _lc.setInsets(ConstraintParser.parseInsets("platform", true));
  
  /** @return (top, left, bottom, right) tuple of UV */
  def getInsets: (UV, UV, UV, UV) = {
    val a = _lc.getInsets
    if (a == null) (N, N, N, N) else
      (UV.toUV(a(0)), UV.toUV(a(1)), UV.toUV(a(2)), UV.toUV(a(3)))
  }
    
  /** 
   * The minimum height for the container. The value will override any 
   * value that is set on the container itself.
   */
  def minHeight(uv: UV) : this.type = { _lc.minHeight(uv.toString); this }
    
  /** 
   * The maximum height for the container. The value will override any 
   * value that is set on the container itself.
   */
  def maxHeight(h: UV) : this.type = { _lc.maxHeight(h.toString); this }
    
  def getWidth : BS = BS.toBS(_lc.getWidth)
    
  def width(w: BS) : this.type = { _lc.width(w.toString); this }
    
  /** 
   * The minimum width for the container. The value will override any 
   * value that is set on the container itself.
   */
  def minWidth(uv: UV) : this.type = { _lc.minWidth(uv.toString); this }
    
  /** 
   * The maximum width for the container. The value will override any 
   * value that is set on the container itself.
   */
  def maxWidth(uv: UV) : this.type = { _lc.maxWidth(uv.toString); this }
  
  /**
   * @param boundSize The default horizontal grid gap between columns.  
   * Defaults to platform default.
   */
  def gapX(boundSize: BS) : this.type = {
    _lc.gridGapX(boundSize.toString)
    this
  }
  /** @return The default grid gap between columns in the grid. */
  def getGapX : Option[BS] = {
    val bs = _lc.getGridGapX
    if (bs == null) None else Some(BS.toBS(bs))
  }

  /**
   * @param boundSize The default vertical grid gap between rows.
   * Defaults to platform default.
   */
  def gapY(boundSize: BS) : this.type = { 
    _lc.gridGapY(boundSize.toString)
    this
  }
  /** @return The default grid gap between rows in the grid. */
  def getGapY : Option[BS] = {
    val bs = _lc.getGridGapY
    if (bs == null) None else Some(BS.toBS(bs))
  }
  
  def gap(boundSizeX: BS, boundSizeY: BS) : this.type = {
    _lc.gridGap(boundSizeX.toString, boundSizeY.toString)
    this
  }
  
  /**
   * The hide mode specified how the layout manager should handle a component
   * that isn't visible. 
   * 1. NoCell - Normal. Bounds will be calculated as if the component was 
   * visible.
   * 2. Visible - If hidden the size will be 0, 0 but the gaps remain.
   * 3. ZeroSquareGaps - If hidden the size will be 0, 0 and gaps set to zero.
   * 4. ZeroSquareNoGaps - If hidden the component will be disregarded 
   * completely and not take up a cell in the grid.
   */
  def hideMode(hideMode: HideMode.Value) = _lc.hideMode(hideMode.id)
  def getHideMode = HideMode(_lc.getHideMode)
  
  /** 
   * Instructs the layout engine to not use caches. This should normally
   * only be needed if the "%" unit is used as it is a function of the
   * parent size. If you are experiencing revalidation problems you can try
   * to set this constraint.
   */
  def noCache : this.type = { _lc.noCache; this }
  def isNoCache = _lc.isNoCache
    
  /**
   * If the whole layout should be non grid based. It is the same as setting
   * the "nogrid" property on every row/column in the grid.
   */
  def noGrid : this.type = { _lc.noGrid; this }
  def isNoGrid = _lc.isNoGrid
  
  /** 
   * Turns off padding of visual bounds (e.g. compensation for drop shadows) 
   * Defaults to true
   */
  def visualPadding(pad: Boolean) : this.type = { 
    _lc.setVisualPadding(pad);
    this 
  }
  def isVisualPadding = _lc.isVisualPadding
    
  /** The underlying java LC instance */
  def java = _lc
    
  override def toString: String = {
    new Out("LC:").
    add("align x", _lc.getAlignX).
    add("align y", _lc.getAlignY).
    add("debug", _lc.getDebugMillis).
    add("top to bottom", _lc.isTopToBottom).
    add("left to right", _lc.getLeftToRight).
    add("fill x", _lc.isFillX).
    add("fill y", _lc.isFillY).
    add("flow x", _lc.isFlowX).
    add("grid gap x", _lc.getGridGapX).
    add("grid gap y", _lc.getGridGapY).
    add("height", _lc.getHeight).
    add("width", _lc.getWidth).
    add("hide mode", HideMode(_lc.getHideMode)).
    add("insets", _lc.getInsets).
    add("no cache", _lc.noCache).
    add("no grid", _lc.noGrid).
    add("visual padding", _lc.isVisualPadding).
    add("wrap after", _lc.getWrapAfter).
    get
  }
}

object LC {
  def apply() : LC = new LC(new net.miginfocom.layout.LC())
}
  
/** 
 *    AAA    CCCC
 *   A   A  C
 *   AAAAA  C      ***********************************************************
 *   A   A  C
 *   A   A   CCCCC
 *
 * Column or row constraints.  Comments will pretend it is a column */
abstract class AC protected[smig] (ac: net.miginfocom.layout.AC) { 
  protected val _ac = ac
  
  private[smig] def this() = this(new net.miginfocom.layout.AC())
  
  /** 
   * For columns, the components in that column will default to a "growX"
   * constraint (which can be overridden by the individual component 
   * constraints). Note that this property does not affect the size for the
   * row, but rather the sizes of the components in the row. 
   */
  def fill : this.type = { _ac.fill; this }
  def fill(indexes: Int*) : this.type = { _ac.fill(indexes: _*); this }
    
  /** Column width as a bound size */
  def size(bs: BS) : this.type = { _ac.size(bs.toString); this }
    
  /** Several column widths as a bound size */
  def size(bs: BS, indexes: Int*) : this.type = {
    _ac.size(bs.toString, indexes: _*)
    this
  }
    
  /** 
   *Specifies that the current row/column should be put in the size group 
   * and will thus share the same size constraints as the other components
   * in the group.
   * @param n Name of group
   */
  def sizeGroup(n: String) : this.type = { _ac.sizeGroup(n); this }
    
  /** 
   *Specifies that the given rows/columns should be put in the size group 
   * and will thus share the same size constraints as the other components
   * in the group.
   * @param n Name of group
   */
  def sizeGroup(n: String, indexes: Int*) : this.type = {
    _ac.sizeGroup(n, indexes: _*)
    this
  }
  
  def getSizeGroup(i: Int) : Option[String] = {
    val grp = _ac.getConstaints()(i).getSizeGroup
    if (grp == null) None else Some(grp)
  }
  
  /** Moves to next column, setting gap */
  def gap(bs: BS) : this.type = { _ac.gap(bs.toString); this }
  
  /** Sets gap after given indexes */
  def gap(bs: BS, indexes: Int*) : this.type = {
    _ac.gap(bs.toString, indexes: _*); this }
    
  /** Specifies the current row/column's grow weight within columns/rows 
   with the same grow priority */
  def grow(g: Float) : this.type = { _ac.grow(g); this }
  def grow : this.type = grow(100.0F)
  def grow(g: Float, indexes: Int*) : this.type = { 
    _ac.grow(g, indexes: _*)
    this
  }
    
  /** Specifies the current row/column's shrink priority. */
  def growPrio(prio: Int) : this.type = { _ac.growPrio(prio); this }
  def growPrio(prio: Int, indexes: Int*) : this.type = {
    _ac.growPrio(prio, indexes: _*)
    this
  }
  
  /** Specifies the current row/column's shrink weight within columns/rows 
   with the same shrink priority */
  def shrink(g: Float) : this.type = { _ac.grow(g); this }
  def shrink : this.type = grow(100.0F)
  def shrink(g: Float, indexes: Int*) : this.type = { 
    _ac.shrink(g, indexes: _*)
    this
  }
    
  /** Specifies the indicated rows'/columns' shrink priority. */
  def shrinkPrio(prio: Int) : this.type = { _ac.shrinkPrio(prio); this }
  def shrinkPrio(prio: Int, indexes: Int*) : this.type = {
    _ac.shrinkPrio(prio,  indexes: _*)
    this
  }
    
  /** Put whole column will be layed out in one cell. */
  def noGrid() : this.type = { _ac.noGrid; this }
  def noGrid(indexes: Int*) : this.type = { _ac.noGrid(indexes: _*); this }
    
    
  /** Set the index of the nect column to specify properties of, starts at
   * 0 
   */
  def index(i: Int) : this.type = {
    require(i >= 0)
    _ac.index(i)
    this
  }
  /** Alias for index */
  def i(i: Int) : this.type = index(i)
  
  /** Total number of rows/columns */
  def count(i: Int) : this.type = {
    require(i >= 0)
    _ac.count(i)
    this
  }
  def getCount = _ac.getCount
    
  def java = _ac
    
  override def toString: String = {
    val out = new Out("AC:")
    val constraints = _ac.getConstaints
    for (i <- 0 until constraints.length) {
      val c = constraints(i)
      out.
      add("Cell", i).                            
      add("align", c.getAlign).
      add("end group", c.getEndGroup).
      add("gap before", c.getGapAfter).
      add("gap after", c.getGapBefore).
      add("grow", c.getGrow).
      add("grow prio", c.getGrowPriority).
      add("shrink weight", c.getShrink).
      add("shrink prio", c.getShrinkPriority).
      add("size", c.getSize).
      add("size group", c.getSizeGroup).
      add("fill", c.isFill).
      add("no grid", c.isNoGrid)
    }
    out.get                       
  }
}
  
/** Row constraint, create with RowC() */
class RowC private[smig] (ac: net.miginfocom.layout.AC) extends AC {
  private[smig] def this() = this(new net.miginfocom.layout.AC())
  
  /** Row's default alignment */
  def align(align: YPos.Value) : this.type = {
    require(align != null)
    _ac.align(align.toString.substring(1))
    this
  }
  def align(align: YPos.Value, indexes: Int*) : this.type = {
    require(align != null)
    _ac.align(align.toString.substring(1), indexes: _*)
    this
  }
}

object RowC {
  def apply() : RowC = new RowC()
}
  
/** Col constraint, create with ColC() */
class ColC private[smig] (ac: net.miginfocom.layout.AC) extends AC {
  private[smig] def this() = this(new net.miginfocom.layout.AC())
  
  /** Column's default alignment */
  def align(align: XPos.Value) : this.type = {
    require(align != null)
    _ac.align(align.toString.substring(1))
    this
  }
  def align(align: XPos.Value, indexes: Int*) : this.type = {
    require(align != null)
    _ac.align(align.toString.substring(1), indexes: _*)
    this
  }
}

object ColC {
  def apply() : ColC = new ColC()
}
    
/** Utility for printing */
private[smig] class Out(s: String) {
  private val _nl = System.getProperty("line.separator")
  private val _sb = new StringBuilder(s).append(_nl)
  private def add(field: String, enum: Enumeration) : this.type = 
    add(field, enum.toString.toLowerCase)
    
  private[smig] def add(field: String, value: Any) : this.type = {
    if (value != null) {
      _sb.append("  ").append(field).append(" = ").append(value).append(_nl)
    }
    this
  }
    
  private[smig] def add(field: String, value: BoundSize) : this.type = {
    if (value != null) {
      add(field, BS.toBS(value))
    }
    this
  }
    
  private[smig] def add(field: String, value: UnitValue) : this.type = {
    if (value != null) {
      add(field, UV.toUV(value))
    }
    this
  }
    
  private[smig] def add(field: String, values: Array[UnitValue]) : this.type = {
    if (values != null) {
      _sb.append("  ").append(field).append(" = [")
      values.map(UV.toUV(_).toString).addString(_sb, ", ")
      _sb.append(']').append(_nl)
    }
    this
  }
    
  def get = _sb.toString
}
  
/** A container preconfigured with a mig layout. You are so lucky. */
class MigPanel private[this] (lc: Option[LC], rowC: Option[RowC], 
                              colC: Option[ColC])
extends Panel with LayoutContainer {
  lazy val getLC : LC = lc match { 
    case None =>  LC() 
    case Some(lc) => lc 
  }
  override lazy val peer =
    new JPanel(new MigLayout(
        getLC.java,
        rowC match { case None => null; case Some(rowC) => rowC.java },
        colC match { case None => null; case Some(colC) => colC.java }
      )) with SuperMixin
  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]
  
  def this(lc: LC, rowC: RowC, colC: ColC) = 
    this(Some(lc), Some(rowC), Some(colC))
  def this(lc: LC, rowC: RowC) = this(Some(lc), Some(rowC), None)
  def this(lc: LC, colC: ColC) = this(Some(lc), None, Some(colC))
  def this(lc: LC) = this(Some(lc), None, None)
  def this(rowC: RowC, colC: ColC) = this(None, Some(rowC), Some(colC))
  def this(rowC: RowC) = this(None, Some(rowC), None)
  def this(colC: ColC) = this(None, None, Some(colC))
  def this() = this(None, None, None)
  
  /** Does the conventional debug, just allows calling it without defining
   * an LC in the constructor.  (One gets created regardless) */    
  def debug(millis: Int) : this.type = {
    layoutManager.getLayoutConstraints().
    asInstanceOf[net.miginfocom.layout.LC].debug(millis)
    this
  }
  
  /**
   * Arrange for useful tool tips telling the constraints are set on the 
   * components
   * @return this
   */
  def debugTip: this.type = {
    ToolTipManager.sharedInstance().setDismissDelay(Int.MaxValue);
    val tip = new StringBuilder("<html>");
    lc match { 
      case Some(lc) => 
        str(lc, tip)
      case _ =>
        tip.append("LC = null<br/>");
    }
    rowC match {
      case Some(rowC) => 
        tip.append("RowC = <br/>")
        str(rowC, tip)
      case _ => tip.append("RowC = null<br/>");
    }
    colC match {
      case Some(colC) => 
        tip.append("ColC = <br/>")
        str(colC, tip)
      case _ => tip.append("ColC = null<br/>");
    }
    tooltip = tip.append("</html>").toString
    addCompTips
    this
  }
  
  private def addCompTips() {
    peer.addContainerListener(new ContListener)
    contents.foreach(setCompTips(_))
  }
  
  private def str(lc: LC, tip: StringBuilder) {
    val l = lc.java
    tip.append("LC<br/>")
    row(tip, "AlignX", l.getAlignX)
    row(tip, "AlignY", l.getAlignY)
    row(tip, "GridGapX", l.getGridGapX)
    row(tip, "GridGapY", l.getGridGapY)
    row(tip, "Height", l.getHeight)
    row(tip, "HideMode", l.getHideMode)
    row(tip, "Insets", l.getInsets)
    row(tip, "LeftToRight", l.getLeftToRight)
    row(tip, "PackHeight", l.getPackHeight)
    row(tip, "PackHeightAlign", l.getPackHeightAlign)
    row(tip, "PackWidth", l.getPackWidth)
    row(tip, "PackWidthAlign", l.getPackWidthAlign)
    row(tip, "Width", l.getWidth)
    row(tip, "WrapAfter", l.getWrapAfter)
    row(tip, "FillX", l.isFillX)
    row(tip, "FillY", l.isFillY)
    row(tip, "FlowX", l.isFlowX)
    row(tip, "NoCache", l.isNoCache)
    row(tip, "NoGrid", l.isNoGrid)
    row(tip, "TopToBottom", l.isTopToBottom)
    row(tip, "VisualPadding", l.isVisualPadding)
  }
  
  private def str(ac: AC, tip: StringBuilder) {
    var i = 0
    ac.java.getConstaints.foreach(c => {
        i += 1
        tip.append(i).append(".<br>")
        row(tip, "Align", c.getAlign)
        row(tip, "EndGroup", c.getEndGroup)
        row(tip, "GapAfter", c.getGapAfter)
        row(tip, "GapBefore", c.getGapBefore)
        row(tip, "Grow", c.getGrow)
        row(tip, "GrowPriority", c.getGrowPriority)
        row(tip, "Shrink", c.getShrink)
        row(tip, "ShrinkPriority", c.getShrinkPriority)
        row(tip, "Size", c.getSize)
        row(tip, "SizeGroup", c.getSizeGroup)
      })
  }
  
  private def setCompTips(comp: Component) {
    val cc = layoutManager.getConstraintMap.get(comp.peer).
    asInstanceOf[net.miginfocom.layout.CC]
    if (cc != null) {
      val tip = new StringBuilder("<html>CC = <br/>")
      row(tip, "CellX", cc.getCellX)
      row(tip, "CellY", cc.getCellY)
      row(tip, "Dock", cc.getDockSide match { 
          case 0 => "North"; case 1 => "West"; case 2 => "South"; 
          case 3 => "East"; case _ => null })
      row(tip, "Pad", str(cc.getPadding))
      row(tip, "PushX", cc.getPushX)
      row(tip, "PushY", cc.getPushY)
      row(tip, "SpanX", cc.getSpanX)
      row(tip, "SpanY", cc.getSpanY)
      row(tip, "Split", cc.getSplit)
      row(tip, "Tag", cc.getTag)
      row(tip, "External", cc.isExternal)
      row(tip, "FlowX", cc.getFlowX)
      row(tip, "Newline", cc.isNewline)
      row(tip, "Wrap", cc.isWrap)
      comp.tooltip = tip.append("</html>").toString
    }
  }
  
  private class ContListener extends ContainerListener() {
    override def componentAdded(ev: ContainerEvent) = 
      SwingUtilities.invokeLater(new Runnable {
          def run() = {
            val child = ev.getChild
            contents.find(c => c.peer == child) match {
              case Some(c) => setCompTips(c)
              case _ => 
            }
          }
        })
    override def componentRemoved(ev: ContainerEvent) = { }
  }
  
  private def row(tip: StringBuilder, name: String, value: Any) {
    if (value != null) {
      if (value.isInstanceOf[Boolean]) {
        if (value.asInstanceOf[Boolean]) {
          tip.append("&nbsp;").append(name).append(" = true").append("<br/>")
        }
      } else if (value.isInstanceOf[UnitValue]) {
        val s = str(value.asInstanceOf[UnitValue])
        if (s != null) tip.append(" = ").append(s).append("<br/>")
      } else if (value.isInstanceOf[BoundSize]) {
        val s = str(value.asInstanceOf[BoundSize])
        if (s != null) tip.append("&nbsp;").append(name).append(" = ").
        append(s).append("<br/>")
      } else {
        tip.append("&nbsp;").append(name).append(" = ").append(value).
        append("<br/>")
      }
    }
  }
  
  private def str(pad: Array[UnitValue]) : String = {
    if (pad == null) null else {
      val sb = new StringBuilder("[");
      pad.map(unitValue => {
          if (unitValue == null) "null" else UV.toUV(unitValue)
        }).addString(sb, ",")
      sb.append("]").toString
    }
  }
  
  private def str(bs: BoundSize) : String = {
    if (bs == null) null else {
      val s = str(bs.getMin) + ":" + str(bs.getPreferred) + ":" +
      str(bs.getMax)
      if ("n:n:n" == s) null else s
    }
  }
  
  private def str(uv: UnitValue) : String = {
    if (uv == null) "n" else { 
      val str = uv.getConstraintString
      if (str == "()") "n" else str
    }
  }
  
  override protected def constraintsFor(comp: Component) = layout(comp)
     
  override protected def areValid(c: Constraints): (Boolean, String) = 
    (true, "Like we really checked")
    
  /**
   * This could be useful if several components share constraints
   * @param com Add this
   * @param con A CC object
   */
  override protected def add(com: Component, con: Constraints) {
    peer.add(com.peer, con._cc)
  }
  
  /**
   * @param com Add this
   * @return a fresh CC for your modifying pleasure
   */
  protected def add(com: Component) : Constraints = {
    val con = new Constraints
    peer.add(com.peer, con._cc)
    con
  }
  
  def getColC : Option[ColC] = {
    val ac = 
      layoutManager.getColumnConstraints.asInstanceOf[net.miginfocom.layout.AC]
    if (ac == null) None else Some(new ColC(ac))
  }
  
  def getRowC : Option[RowC] = {
    val ac =  
      layoutManager.getRowConstraints.asInstanceOf[net.miginfocom.layout.AC]
    if (ac == null) None else Some(new RowC(ac))
  }
  
  /** Pass 2 functions that will be used to create a LayoutCallback */
  def addLayoutCallback(getSize: (ComponentWrapper) => Array[BoundSize],
                        correctBounds: (ComponentWrapper) => Unit) {
    layoutManager.addLayoutCallback(new LayoutCallback() {
        override def getSize(comp: ComponentWrapper) : Array[BoundSize] =
          getSize(comp)
        override def correctBounds(comp: ComponentWrapper) : Unit =
          correctBounds(comp)
      })
  }
  
  private[smig] def cc = new CC()
  
  /** 
   *   CCCC    CCCC
   *  C       C
   *  C       C      ***********************************************************
   *  C       C
   *   CCCCC   CCCCC
   *
   * Component constraints (Subclass of MigPanel you observe) */
  
  type CC = Constraints // more natural for mig users
  
  class Constraints private[smig] (cc: net.miginfocom.layout.CC) {
    private[smig] var _cc = cc
    
    def this() = this(new net.miginfocom.layout.CC())
    
    def alignX(a: AlignX) : this.type = { _cc.alignX(a._a); this }
    
    def alignY(a: AlignY) : this.type = { _cc.alignY(a._a); this }
  
    def align(aX: AlignX, aY: AlignY) : this.type = {
      _cc.alignX(aX._a)
      _cc.alignY(aY._a)
      this
    }
    
    /**
     * @param x coord
     * @param y coord
     * @return this
     */
    def cell(x: Int, y: Int) : this.type = { _cc.cell(x, y); this }
    /**
     * @param x coord
     * @return this
     */
    def cellX(x: Int) : this.type = { _cc.setCellX(x); this }
    def getCellX : Int = _cc.getCellX
    
    /**
     * @param y coord
     * @return this
     */
    def cellY(y: Int) : this.type = { _cc.setCellY(y); this }
    def getCellY : Int = _cc.getCellY
    
    /** 
     * For a component to take up the whole side, unless something
     * else is docked.  Operates beyond cells
     * @return this
     */
    def dock(dock: Dock.Value) : this.type = {
      require(dock != null)
      dock match {
        case Dock.North => _cc.dockNorth
        case Dock.West => _cc.dockWest
        case Dock.South => _cc.dockSouth
        case Dock.East => _cc.dockEast
      } 
      this
    }
    def getDock : Dock.Value = Dock(_cc.getDockSide)
    
    /** 
     * Specifies that the component should be put in the end group grp and
     * will thus share the same ending coordinate as them within the group. 
     * @return this
     */
    def endGroupX(grp: String) : this.type = { _cc.endGroupX(grp); this }
    def endGroupY(grp: String) : this.type = { _cc.endGroupY(grp); this }
    
    def external : this.type = { _cc.external; this }
    def isExternal : Boolean = _cc.isExternal
    
    /**
     * Convenience method for what is required to make something fill space in
     * the X direction.  It is shorthand for growX.pushX
     */
    def fillX : this.type = { _cc.growX; _cc.pushX; this }
    /**
     * Convenience method for what is required to make something fill space in
     * the X direction.  It is shorthand for growX.pushX
     */
    def fillY : this.type = { _cc.growY; _cc.pushY; this }
    
    /** Flow direction within cell */
    def flowX : this.type = { _cc.setFlowX(true);  this }
    def flowY : this.type = { _cc.setFlowX(false);  this }
    def isFlowX : Boolean = { 
      _cc.getFlowX match {
        case null => true
        case _ => flowX.asInstanceOf[Boolean]
      }
    }
    
    /** Gap above a component */
    def gapTop(bs: BS) : this.type = { _cc.gapTop(bs.toString); this }
    /** Gap left of a component */
    def gapLeft(bs: BS) : this.type = { _cc.gapLeft(bs.toString); this }
    /** Gap below a component */
    def gapBottom(bs: BS) : this.type = { _cc.gapBottom(bs.toString); this }
    /** Gap right of a component */
    def gapRight(bs: BS) : this.type = { _cc.gapRight(bs.toString); this }
    
    /** X growth weight for when several items in cell, default 100F */
    def growX : this.type = growX(100.0F)
    def growX(g: Float) : this.type = {
      require(g >= 0.0F)
      _cc.growX(g)
      this
    }
    
    /** Y growth weight for when several items in cell, default 100F */
    def growY : this.type = growY(100.0F)
    def growY(g: Float) : this.type = {
      require(g >= 0.0F)
      _cc.growY(g)
      this
    }
    
    def growPrioX(i: Int) : this.type = {
      require(i >= 0)
      _cc.growPrioX(i)
      this
    }
    
    /** The grow priority compared to other components in the same cell. */
    def growPrioY(i: Int) : this.type = {
      require(i >= 0)
      _cc.growPrioY(i)
      this
    }
    
    /** The minimum width for the component. The value will override any value 
     that is set on the component itself. */
    def minWidth(uv: UV) : this.type = { _cc.minWidth(uv.toString); this }
    
    /** The width for the component as a bound size */
    def width(bs: BS) : this.type = { _cc.width(bs.toString); this }
    
    /** The maximum width for the component. The value will override any value 
     that is set on the component itself. */
    def maxWidth(uv: UV) : this.type = { _cc.maxWidth(uv.toString); this }
    
    /** The minimum height for the component. The value will override any value 
     that is set on the component itself. */
    def minHeight(uv: UV) : this.type = { _cc.minHeight(uv.toString); this }
    
    /** The height for the component as a bound size */
    def height(bs: BS) : this.type = { _cc.height(bs.toString); this }
    
    /** The maximum height for the component. The value will override any value 
     that is set on the component itself. */
    def maxHeight(uv: UV) : this.type = { _cc.maxHeight(uv.toString); this }
   
    /** 
     * Go to next row/col before component.  The wrapped class allows adding
     * a gap at the same time but I say use the gap functions.
     */
    def newline : this.type = { _cc.newline; this }
    def isNewline : Boolean = _cc.isNewline
    
    def pad(top: UV, left: UV, bottom: UV, right: UV) : this.type = {
      _cc.setPadding(Array[UnitValue](top._value, left._value, 
                                      bottom._value, right._value))
      this
    }
    def pad(f: Float) : this.type = { 
      val uv = UV.toUV(f)
      pad(uv, uv, uv, uv); 
      this 
    }
    /** @return the absolute resizing in the last stage of the layout cycle. */
    def getPad() : Option[(UV, UV, UV, UV)] = {
      val pad = _cc.getPadding()
      if (pad == null) None else {
        Some((UV.toUV(pad(0)), UV.toUV(pad(1)),
              UV.toUV(pad(2)), UV.toUV(pad(3))))
      }
    }
    
    /**
     * "pushX" indicates that the column that this component is in (the first
     * if the component spans several) should default to growing.  If any column
     * has been set to push, this value on the component does nothing as
     * column push takes precedence. 
     */
    def pushX(weight: Float) : this.type = { _cc.pushX(weight); this }
    def getPushX : Option[Float] = {
      val f = _cc.getPushX
      if (f == null) None else Some(f.asInstanceOf[Float])
    }
    
    /**
     * "pushY" indicates that the row that this component is in (the first
     * if the component spans) should default to growing.  If any row
     * has been set to push, this value on the component does nothing as
     * row push takes precedence.
     */
    def pushY(weight: Float) : this.type = { _cc.pushY(weight); this }
    def getPushY : Option[Float] = {
      val f = _cc.getPushY
      if (f == null) None else Some(f.asInstanceOf[Float])
    }
    
    /** Same as pushX(100.0F) */
    def pushX : this.type = pushX(100.0F)
    /** Same as pushY(100.0F) */
    def pushY : this.type = pushY(100.0F)
    /** Same as pushX, pushY */
    def push : this.type = { _cc.pushX; _cc.pushY; this }
    
    /** Shrink weight for the component */
    def shrinkX(g: Float) : this.type = {
      require(g >= 0.0F)
      _cc.shrinkX(g)
      this
    }
    
    /** Shrink weight for the component */
    def shrinkY(g: Float) : this.type = {
      require(g >= 0.0F)
      _cc.shrinkY(g)
      this
    }
    
    /** The shrink priority compared to other components IN THE SAME CELL. */
    def shrinkPrioX(i: Int) : this.type = {
      require(i >= 0)
      _cc.shrinkPrioX(i)
      this
    }
    
    /** The shrink priority compared to other components IN THE SAME CELL. */
    def shrinkPrioY(i: Int) : this.type = {
      require(i >= 0)
      _cc.shrinkPrioY(i)
      this
    }
    
    /** 
     * Specifies that the component should be put in the size group and will
     * thus share the size with others in the group. 
     */
    def sizeGroupX(grp: String) : this.type = { _cc.sizeGroupX(grp); this }
    def sizeGroupY(grp: String) : this.type = { _cc.sizeGroupY(grp); this }
    
    /** Skip this many cells in flow direction before placing. */
    def skip(i: Int) : this.type = { _cc.skip(i); this }
    
    /**
     * Span completely in X direction
     */
    def spanX : this.type = { _cc.spanX; this }
    /**
     * The number of cells the cell that this constraint's component
     * will span in the X dimension.
     */
    def spanX(w: Int) : this.type = { _cc.spanX(w); this }
    def getSpanX : Int = _cc.getSpanY
    
    /**
     * Span completely in Y direction
     */
    def spanY : this.type = { _cc.spanY; this }
    /**
     * The number of cells the cell that this constraint's component
     * will span in the Y dimension.
     */
    def spanY(h: Int) : this.type = { _cc.spanY(h); this }
    def getSpanY : Int = _cc.getSpanY
    
    /** 
     * Sets in how many parts the current cell (that this constraint's 
     * component will be in) should be split in. If for instance it is split
     * in two, the next component will also share the same cell. Note that the
     * cell can also span a number of cells, which means that you can for
     * instance span three cells and split that big cell for two components.
     * Split can be set to a very high value to make all components in the
     * same row/column share the same cell.
     * 
     * Note that only the first component will be checked for this property.
     * 
     * @param count The number of parts (i.e. component slots) the cell 
     * should be divided into.
     */
    def split(count: Int) : this.type = { _cc.split(count); this }
    /**
     * @return this
     */
    def getSplit : Int = _cc.getSplit
    
    def tag(t: String) : this.type  = { _cc.tag(t); this }
    /**
     * @return The tag string
     */
    def getTag : Option[String] = {
      val tag = _cc.getTag
      if (tag == null) None else Some(tag)
    }
    
    /** 
     * Go to next row/col after component.  The wrapped class allows adding
     * a gap at the same time but I say use the gap functions.
     */
    def wrap : this.type = { _cc.wrap; this }
    def isWrap = _cc.isWrap
    
    /** The java peer */
    def java = _cc
  }
  
  object Util {
    private var _groupNum: Int = 0;
    /* MigLayout does not define constants for the hidemode parameters. */
    val HIDEMODE_NORMAL_SIZE = 0;
    val HIDEMODE_ZERO_SIZE = 1;
    val HIDEMODE_ZERO_SIZE_NO_GAP = 2;
    val HIDEMODE_NO_PARTICIPATION = 3;
    val _HUGE_DIST = LayoutUtil.INF;
    val _TINY_DIM = new Dimension(0, 0);
    val _HUGE_DIM = new Dimension(_HUGE_DIST, _HUGE_DIST);
    val _HORZ_HUGE_DIM = new Dimension(_HUGE_DIST, 0);
    val _VERT_HUGE_DIM = new Dimension(0, _HUGE_DIST);
    val _NARROW = 3;
    val CLEAR: Color = new Color(0, true)

    /** Assume done in awt dispatch thread */
    def getGroupName : String = "MigUtilBtnGroup_" + (_groupNum += 1);

    /**
     * The Layout should already be on the correct cell, probably as the
     * result of a wrap
     * @param container add in this container which is using MigLayout
     * @param btns add this components, centered on row and with same width
     */
    def addBtnRow(migPanel: MigPanel, sameSize: Boolean, 
                  btns: Component*) : Unit = {
      addBtnRow(-1, migPanel, sameSize, btns:_*)
    }

    /** 
     * Flow in layout should be X
     * @param row Add on this row unless < 0
     * @param container add in this container which is using MigLayout
     * @param sameSize true to coerce buttons (or whatever) to same size
     * @param btns add these components, centered on row and with same width
     */
    def addBtnRow(row: Int, migPanel: MigPanel, sameSize: Boolean,
                  btns: Component*) {
      require(migPanel.getLC.isFlowX)
      val spaceSizeGroup = getGroupName
      var btnSizeGroup: String = null
      if (sameSize) {
        btnSizeGroup = getGroupName
      }
      val cc = migPanel.cc
      if (row >= 0) {
        cc.cell(0, row)
      }
      val bs0 = BS.toBS(0)
      migPanel.add(new Pad()).spanX.split(btns.length * 2 + 3).
      flowX.sizeGroupX(spaceSizeGroup).gapLeft(bs0).gapRight(bs0).
      gapTop(bs0).gapBottom(bs0).growX
      val cs = migPanel.cc.sizeGroupX(spaceSizeGroup).
      gapLeft(bs0).gapRight(bs0).gapTop(bs0).gapBottom(bs0).fillX
      val cb = migPanel.cc.gapLeft(bs0).gapRight(bs0).gapTop(bs0).gapBottom(bs0)
      if (sameSize) {
        cb.sizeGroupX(btnSizeGroup)
      }
      btns.foreach(comp => {
          migPanel.add(new Pad(), cs)
          migPanel.add(comp, cb)
        })
      migPanel.add(new Pad(), cs)
      migPanel.add(new Pad(), cs)
    }

    private[smig] class Pad extends Component {
      background = CLEAR
    }

    /**
     * Create a transparent {@code component} with huge maximum width.
     * Using it with .fill will make it take up space.
     *
     * @return A transparent horizontally springy component
     */
    def createHorzSpring : Spring = new Spring(Direction.X)

    def createVertSpring : Spring = new Spring(Direction.Y)

    def create2WaySpring : Spring = new Spring(Direction.XY)

    object Direction extends Enumeration {
      val X, Y, XY = Value
    }

    class Spring private[smig] (direction: Direction.Value) extends JPanel {
      protected val _direction = direction
      protected var _debug: Boolean = _
      background = CLEAR
      
      private def dim(dim: Dimension) : Dimension = {
        if (_debug) new Dimension(
          if (dim.width < _NARROW) _NARROW else dim.width, 
          if (dim.height < _NARROW) _NARROW else dim.height) 
        else dim;
      }
      
      def debug: Spring = {
        _debug = true
        name = _direction.toString + " Spring"
        opaque = true
        background = new Color((Color.RED.getRGB() & (~0 >> 8)) | (0x80 << 24))
        this
      }

      override def getMinimumSize: Dimension = dim(_TINY_DIM)
      override def getPreferredSize: Dimension = dim(_TINY_DIM)

      override def getMaximumSize: Dimension = {
        _direction match {
          case Direction.X => dim(_HORZ_HUGE_DIM);
          case Direction.Y => dim(_VERT_HUGE_DIM);
          case _ => dim(_HUGE_DIM);
        }
      }
    }

    class Strut(direction: Direction.Value, size: Int) extends JPanel {
      val _direction: Direction.Value = direction
      val _size = size
      var _debug : Boolean = _

      def debug : Strut = {
        _debug = true
        name = _direction.toString + " Strut"
        opaque = true
        background = new Color((Color.RED.getRGB() & (~0 >> 8)) | (0x80 << 24))
        this;
      }

      override def getMinimumSize: Dimension = {
        _direction match {
          case Direction.X => new Dimension(_size, if (_debug) _NARROW else 0)
          case Direction.Y => new Dimension(if (_debug) _NARROW else 0, _size)
          case _ => null 
        }
      }

      override def getPreferredSize: Dimension = getMinimumSize

      override def getMaximumSize: Dimension = {
        _direction match {
          case Direction.X => 
            new Dimension(Short.MaxValue, if (_debug) _NARROW else 0)
          case Direction.Y => 
            new Dimension(if (_debug) _NARROW else 0, Short.MaxValue);
          case _ => null
        }
      }
    }
  }
}