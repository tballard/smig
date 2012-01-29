/*
 * srcbin
 */

package smig

import net.miginfocom.swing.MigLayout
import net.miginfocom.layout.{
  ComponentWrapper,
  ConstraintParser,
  LayoutCallback,
  LayoutUtil,
  UnitValue
}
import java.awt.{ 
  Color, 
  Dimension 
}
import java.awt.event.{ 
  ContainerEvent, 
  ContainerListener 
}
import javax.swing.{
  JComponent,
  JLabel,
  JPanel,
  SwingUtilities,
  ToolTipManager
}
import net.miginfocom.layout.BoundSize
import scala.collection.JavaConversions._
import scala.swing.{
  Component,
  LayoutContainer,
  Panel
}
import scala.collection.mutable.WeakHashMap

/**
 * First things first.  A smig is a kind of beard.  Sorta the goatee motif.
 * Makes you look mighty sporty, similar to what this will do to your UI.
 * I guess it means a lot of other things, too, according to the slang 
 * dictionary.  Like this is smiggin' cool, dude.
 *
 * This is a wrapper for the mig layout manager for use from Scala.
 * It was designed to make things more type-safe than the java version
 * because, although not personally a nazi, I prefer APIs that are.
 *
 * Ready for production use?  As ready as a bunch of stuff you are already
 * using.  Pitch it in your project, patch it if it needs it and tell me
 * about it.
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
 * And easier to use from java.
 * 
 * Options are used where nulls may be exposed.
 *
 * I rethought, and I believe, improved the cell selection API.  The calls
 * you know and love may have been replaced by counterparts in MigPanel.
 * 
 * The callbacks have been implemented in such a way as to allow submitting 
 * them individually, seeing as functions are objects and all.
 * 
 * Examples of my '''fascism''' include:
 *  
 * * Enumerations for Dock, XPos, YPos, HideMode, Tag
 * 
 * * The ability to write arithmetic expressions in the specification strings
 *   is replaced with arithmetic operations on classes representing unit values.
 *   They try to make sense.  For instance there can only be one alignment in
 *   an expression and you can only add to or subtract from it.  Multiplication
 *   and division is by numbers only.
 *   
 * * All UnitValues are derived classes of UV, e.g. MM(25) or PCT(25)
 *
 * tom@geeksite.org
 */

/** Specify 4 possible dock positions in component constraint */
object Dock extends Enumeration {
  type Dock = Value
  val North, West, South, East = Value
  private[smig] implicit def toString(dock: Value) =
    dock.toString.toLowerCase
}
import Dock._

/**
 * These are specialized X alignments. Substring(1) of the name is the
 * string MigLayout uses
 */
object XPos extends Enumeration {
  type XPos = Value
  val Xleft, Xright, Xcenter, Xleading, Xtrailing, Xbaseline = Value
} 
import XPos._

/** These are specialized Y alignments */
object YPos extends Enumeration {
  type YPos = Value
  val Ytop, Ybottom, Ycenter, Yleading, Ytrailing, Ybaseline = Value
}
import YPos._

/** Uses for tell behavior when component is hidden. */
object HideMode extends Enumeration {
  type HideMode = Value
  val Visible, ZeroSquareGaps, ZeroSquareNoGaps, NoCell = Value
}
import HideMode._

object Tag extends Enumeration {
  type Tag = Value
  val Ok, Cancel, Help, Help2, Yes, No, Apply, Back,
    Next, Finish, Left, Right = Value
  private[smig] implicit def toString(tag: Value) =
    tag.toString.toLowerCase
}
import Tag._

/** 
 * No constructor for you, but XPos. Float, PCT, Int work as well as
 * the vals AlignX.{ZERO_X, CENTER_X, LEFT, RIGHT, LEADING_X, TRAILING_X} 
 */
class AlignX private[smig] (a: String) { 
  override def toString = a 
}
object AlignX {
  implicit def toAlignX(p: XPos) =
    new AlignX(p.toString.substring(1))
  implicit def toAlignX(f: Float) = new AlignX(f.toString)
  implicit def toAlignX(p: PCT) = new AlignX(p.value.getValue + "%")
  implicit def toAlignX(uv: UV) = new AlignX(uv.toString)
  implicit def toAlignX(uv: UnitValue) =
    new AlignX(uv.getValue + UV.typeString(uv.getUnit))

  lazy val ZERO_X = AlignX.toAlignX(PX(0))
  lazy val CENTER_X = AlignX.toAlignX(PCT(50))
  lazy val LEFT = AlignX.toAlignX(PCT(0))
  lazy val RIGHT = AlignX.toAlignX(PCT(100))
  lazy val LEADING_X = AlignX.toAlignX(PCT(0))
  lazy val TRAILING_Y = AlignX.toAlignX(PCT(100))
}
/** 
 * No constructor for you, but YPos. Float, PCT, Int work as well as
 * the vals AlignX.{ZERO_Y, CENTER_Y, LEFT, RIGHT, LEADING_Y, TRAILING_Y} 
 */
class AlignY private[smig] (a: String) {
  override def toString = a
}
object AlignY {
  implicit def toAlignY(p: YPos) =
    new AlignY(p.toString.substring(1))
  implicit def toAlignY(f: Float) = new AlignY(f.toString)
  implicit def toAlignY(p: PCT) = new AlignY(p.value.getValue + "%")
  implicit def toAlignY(uv: UV) = new AlignY(uv.toString)
  implicit def toAlignY(uv: UnitValue) =
    new AlignY(uv.getValue + UV.typeString(uv.getUnit))

  lazy val ZERO_Y = AlignY.toAlignY(PX(0))
  lazy val CENTER_Y = AlignY.toAlignY(PCT(50))
  lazy val TOP = AlignY.toAlignY(PCT(0))
  lazy val BOTTOM = AlignY.toAlignY(PCT(100))
  lazy val LEADING_X = AlignY.toAlignY(PCT(0))
  lazy val TRAILING_Y = AlignY.toAlignY(PCT(100))
}

/**
 *   U    U V   V
 *   U    U V   V
 *   U    U V   V **************************************************************
 *   U    U  V V  
 *    VVVV    V
 *
 * Implements the Mig UnitValue
 *
 * Privately constructed base class for unit values.  Use the derived
 * classes for strong typing.  Example: PX(2)
 * Immutable
 */
class UV private[smig] (str: String, isHor: Boolean) {
  private[smig] lazy val value: UnitValue = {
    val sn = getClass.getSimpleName
    ConstraintParser.parseUnitValue(str, isHor)
  }
  private[smig] def this(unitValue: UnitValue) = 
    this(unitValue.getConstraintString(), unitValue.isHorizontal)

  override def toString: String = str
  def +(uv: UV_Arith) : UV = new UV(str + "+" + uv.toString, isHor)
  def -(uv: UV_Arith) : UV = new UV(str + "-" + uv.toString, isHor)
  def +(f: Float) : UV = new UV(str + "+" + f, isHor)
  def -(f: Float) : UV = new UV(str + "-" + f, isHor)
}

object UV {
  /** "Infinite" UV */
  lazy val INF = toUV(LayoutUtil.INF)
  implicit def toUV(f: Float): UV = PX(f)
  implicit def toUV(i: Int): UV = PX(i.floatValue)
  implicit def toUV(unitValue: UnitValue): UV = {
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
  implicit def toUnitValue(uv: UV) = uv.value

  private[smig] def typeString(i: Int): String = {
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

/** These may be used in arithmetic expressions operating on other UVs */
class UV_Arith(str: String, isHor: Boolean) extends UV(str, isHor) {
  def *(f: Float) : UV = new UV("(" + str + ")*" + f, isHor)
  def /(f: Float) : UV = new UV("(" + str + ")/" + f, isHor)
}

/** UV for align. strongly typed AL(25) becomes "25.0al" */
final class AL private (f: Float) extends UV(f + "al", false)
object AL { def apply(a: Float) = new AL(a) }

/** Specialize UV for logical pixels so spaces try to grow with fonts. */
final class LPX private (f: Float) extends UV_Arith(f + "lp", true)
object LPX { def apply(f: Float) = new LPX(f) }

/** Specialize UV for logical pixels so spaces try to grow with fonts. */
final class LPY private (f: Float) extends UV_Arith(f + "lp", false)
object LPY { def apply(f: Float) = new LPY(f) }

/** UV for percent. strongly typed PCT(25) becomes "25.0%" */
final class PCT private (percent: Float) extends UV(percent + "%", false)
object PCT { def apply(percent: Float) = new PCT(percent) }

/** Specialize UV for pixels */
final class PX private (f: Float) extends UV_Arith(f + "px", true)
object PX { def apply(f: Float): PX = new PX(f) }

/** Specialize UV for, you know, Screen Percentage */
final class SPX private (f: Float) extends UV_Arith(f + "spx", true)
object SPX { def apply(f: Float): SPX = new SPX(f) }

/** Specialize UV for, you know, Screen Percentage */
final class SPY private (f: Float) extends UV(f + "spy", false)
object SPY { def apply(f: Float): SPY = new SPY(f) }

/** Extend UV for inputting inches */
final class IN private (f: Float) extends UV_Arith(f + "in", false)
object IN { def apply(f: Float) = new IN(f) }

/** Extend UV for inputting millimeters */
final class MM private (f: Float) extends UV_Arith(f + "mm", false)
object MM { def apply(f: Float) = new MM(f) }

/** Extend UV for inputting centimeters */
final class CM private (f: Float) extends UV_Arith(f + "cm", false)
object CM { def apply(f: Float) = new CM(f) }

/** Container X1 for use in expressions */
object ContX1 extends UV_Arith("container.x", true)

/** Container X2 for use in expressions */
object ContX2 extends UV_Arith("container.x2", true)

/** Container Y1 for use in expressions */
object ContY1 extends UV_Arith("container.y", false)

/** Container Y2 for use in expressions */
object ContY2 extends UV_Arith("container.y2", false)

/** Container Width for use in expressions */
object ContW extends UV_Arith("container.w", true)

/** Container Height for use in expressions */
object ContH extends UV_Arith("container.h", false)

/** Container screen X for use in expressions */
object ContXPos extends UV_Arith("container.xpos", true)

/** Container screen Y for use in expressions */
object ContYPos extends UV_Arith("container.ypos", false)

/** Container X1 minus insets for use in expressions */
object VisualX1 extends UV_Arith("visual.x", true)

/** Container X2 minus insets for use in expressions */
object VisualX2 extends UV_Arith("visual.x2", true)

/** Container Y1 minus insets for use in expressions */
object VisualY1 extends UV_Arith("visual.y", false)

/** Container Y2 minus insets for use in expressions */
object VisualY2 extends UV_Arith("visual.y2", false)

/** Container Width minus insets for use in expressions */
object VisualW extends UV_Arith("visual.w", true)

/** Container Height minus insets for use in expressions */
object VisualH extends UV_Arith("visual.h", false)

/** Container screen X minus insets for use in expressions */
object VisualXPos extends UV_Arith("visual.xpos", true)

/** Container screen Y minus insets for use in expressions */
object VisualYPos extends UV_Arith("visual.ypos", false)

/** Component min size for use in expressions */
object Min extends UV_Arith("min", false)

/** Component max size for use in expressions */
object Max extends UV_Arith("max", false)

/** Component preferred size for use in expressions */
object Pref extends UV_Arith("pref", false)

/** Platform specific X spacing for related objects. */
object Rel extends UV_Arith("rel", true)
/** Platform specific Y spacing for related objects. */
object Unrel extends UV_Arith("unrel", true)
/** Platform specific X spacing for unrelated objects. */
object ButtonMin extends UV_Arith("button", false)
/** Platform specific standard indent. */
object Indent extends UV_Arith("indent", false)


/** X1 of component, from related ID, for use in expressions. */
final class IdX1 private[smig] (id: ID) extends UV_Arith(id + ".x", true)
/** Y1 of component, from related ID, for use in expressions. */
final class IdY1 private[smig] (id: ID) extends UV_Arith(id + ".y", false)
/** X2 of component, from related ID, for use in expressions. */
final class IdX2 private[smig] (id: ID) extends UV_Arith(id + ".x2", true)
/** Y2 of component, from related ID, for use in expressions. */
final class IdY2 private[smig] (id: ID) extends UV_Arith(id + ".y2", false)

/** Unique identifier for component, used in expressions */
final class ID private[smig] (id: String) {
  override def toString = id
  
  /** X1 of component, from related ID, for use in expressions. */
  def x1 = new IdX1(this)
  /** Y1 of component, from related ID, for use in expressions. */
  def y1 = new IdY1(this)
  /** X2 of component, from related ID, for use in expressions. */
  def x2 = new IdX2(this)
  /** Y2 of component, from related ID, for use in expressions. */
  def y2 = new IdY2(this)
}

/** 
 * FS means freestyle.  Allow inputting any old crap without any checking.
 * Go hang yourself.
 */
final class FS private (s: String) extends UV(s, false)
object FS { def apply(s: String) = new FS(s) }

/** Extend UV for null field in BS */
final object N extends UV("n", false)

/** Extend UV for copying "pref" field in BS. Usage BS(S, 20, S) */
final object S extends UV("Same", false)

/**
 *   BBBBB   SSSS
 *   B    B S
 *   BBBBB   SSSS  *************************************************************
 *   B    B      S
 *   BBBBBB SSSSS
 *
 * Implements the Mig BoundSize
 */
class BS(min: UV, pref: UV, max: UV, push: Boolean) {
  val getMin: UV = defaultToPref(min)
  val getPref: UV = if (pref == null) N else pref
  val getMax: UV = defaultToPref(max)
    
  private def defaultToPref(uv: UV) : UV = 
    uv match { case S => pref; case null => N; case _ => min; }
  
  def isPush = push

  /** @return The string that could be used in java */
  override def toString(): String = {
    val sb = new StringBuilder().append(min).append(':').append(pref).
      append(':').append(max)
    if (push) sb.append(':').append("push")
    sb.toString()
  }
}

/* Conversions, creation methods for Bound Size. */
object BS {
  /** All nulls */
  def apply(): BS = new BS(N, N, N, false)
  
  def apply(pref: UV): BS = new BS(N, pref, N, false)
  def apply(pref: UV, push: Boolean): BS = new BS(N, pref, N, push)
  def apply(min: UV, pref: UV): BS = new BS(min, pref, null, false)
  def apply(min: UV, pref: UV, push: Boolean): BS = new BS(min, pref, null, push)
  def apply(min: UV, pref: UV, max: UV): BS = new BS(min, pref, max, false)
  def apply(min: UV, pref: UV, max: UV, push: Boolean): BS = new BS(min, pref, max, push)

  /** Convert an Int to a BS */
  implicit def toBS(i: Int): BS = BS(UV.toUV(i))

  /** Convert a UV to a BS */
  implicit def toBS(uv: UV): BS = BS(uv)
  /** Convert a UnitValue to a BS */
  implicit def toBS(unitValue: UnitValue): BS = toBS(UV.toUV(unitValue))

  /** Convert a java BoundSize to a scala BS */
  implicit def toBS(boundSize: BoundSize): BS = 
    BS(
      UV.toUV(boundSize.getMin),
      UV.toUV(boundSize.getPreferred),
      UV.toUV(boundSize.getMax))

  implicit def toBoundSize(bs: BS): BoundSize =
    new BoundSize(bs.getMin.value, bs.getPref.value, bs.getMax.value,
                  bs.isPush, null)
}

/**
 *   L       CCCC
 *   L      C
 *   L      C      *************************************************************
 *   L      C
 *   LLLLLL  CCCCC
 *
 * Layout constraints thin wrapper
 */
class LC private[smig] (lc: net.miginfocom.layout.LC) {
  private val _lc = lc

  /** An XPos, Float, PCT or UV will be converted */
  def alignX(align: AlignX): this.type = {
    require(align != null)
    _lc.alignX(align.toString)
    this
  }

  /** A YPos. Float, PCT or UV will be converted */
  def alignY(align: AlignY): this.type = {
    require(align != null)
    _lc.alignY(align.toString)
    this
  }

  def getAlignX: Option[AlignX] = {
    val unitValue = _lc.getAlignX
    if (unitValue == null) None else
      Some(AlignX.toAlignX(unitValue.getValue))
  }

  def getAlignY: Option[AlignY] = {
    val unitValue = _lc.getAlignY
    if (unitValue == null) None else
      Some(AlignY.toAlignY(unitValue.getValue))
  }

  /** Glory in the convenience. */
  def align(ax: AlignX, ay: AlignY): this.type = {
    alignX(ax)
    alignY(ay)
  }

  /**
   * Specifies if the components should be added in the grid bottom-to-top
   * or top-to-bottom. This value is not picked up from the container and is
   * top-to-bottom by default.
   */
  def bottomToTop(upFlag: Boolean): this.type = {
    if (upFlag) _lc.bottomToTop else _lc.topToBottom;
    this
  }

  /**
   * Overrides the container's ComponentOrientation property for this layout.
   * Normally this value is dependent on the Locale that the application is
   * running. This constraint overrides that value.
   */
  def leftToRight(l: Boolean): this.type = { _lc.leftToRight(l); this }

  /**
   * Claims all available space in the container for the columns and/or
   * rows. At least one component need to have a "grow" constraint for it to
   * fill the container. The space will be divided equally, though honoring
   * "growPriority". If no column/row has "grow" set the grow weight of
   * the components in the rows/columns will migrate to that row/column.
   */
  def fillX: this.type = { _lc.fillX; this }
  def isFillX = _lc.isFillX
  def fillY: this.type = { _lc.fillY; this }
  def isFillY = _lc.isFillY
  def fill: this.type = { fillX; fillY; this }

  def getHeight: BS = BS.toBS(_lc.getHeight)

  /* 
   * The height for the container as a BS. The value will override any value
   * that is set on the container itself.
   */
  def height(h: BS): this.type = { _lc.height(h.toString); this }

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
    right: Option[UV]): this.type = {
    val oldInsets: Array[UnitValue] = _lc.getInsets
    if (oldInsets == null) {
      def str(uv: Option[UV]): String =
        { uv match { case None => null; case Some(x) => x.toString } }
      _lc.insets(str(top), str(left), str(bottom), str(right))
    } else {
      def better(o: UnitValue, uv: Option[UV]): String =
        { (uv match { case None => N; case _ => uv }).toString }
      _lc.insets(better(oldInsets(0), top), better(oldInsets(1), left),
        better(oldInsets(2), bottom), better(oldInsets(3), right));
    }
    this
  }
  def insets(inset: UV): this.type = {
    val i: Option[UV] = Some(inset)
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
  def minHeight(uv: UV): this.type = { _lc.minHeight(uv.toString); this }

  /**
   * The maximum height for the container. The value will override any
   * value that is set on the container itself.
   */
  def maxHeight(h: UV): this.type = { _lc.maxHeight(h.toString); this }

  def getWidth: BS = BS.toBS(_lc.getWidth)

  def width(w: BS): this.type = { _lc.width(w.toString); this }

  /**
   * The minimum width for the container. The value will override any
   * value that is set on the container itself.
   */
  def minWidth(uv: UV): this.type = { _lc.minWidth(uv.toString); this }

  /**
   * The maximum width for the container. The value will override any
   * value that is set on the container itself.
   */
  def maxWidth(uv: UV): this.type = { _lc.maxWidth(uv.toString); this }

  /**
   * @param boundSize The default horizontal grid gap between columns.
   * Defaults to platform default.
   */
  def gapX(boundSize: BS): this.type = {
    _lc.gridGapX(boundSize.toString)
    this
  }
  /** @return The default grid gap between columns in the grid. */
  def getGapX: Option[BS] = {
    val bs = _lc.getGridGapX
    if (bs == null) None else Some(BS.toBS(bs))
  }

  /**
   * @param boundSize The default vertical grid gap between rows.
   * Defaults to platform default.
   */
  def gapY(boundSize: BS): this.type = {
    _lc.gridGapY(boundSize.toString)
    this
  }
  /** @return The default grid gap between rows in the grid. */
  def getGapY: Option[BS] = {
    val bs = _lc.getGridGapY
    if (bs == null) None else Some(BS.toBS(bs))
  }

  def gap(boundSizeX: BS, boundSizeY: BS): this.type = {
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
  def hideMode(hideMode: HideMode) = _lc.hideMode(hideMode.id)
  def getHideMode = HideMode(_lc.getHideMode)

  /**
   * Instructs the layout engine to not use caches. This should normally
   * only be needed if the "%" unit is used as it is a function of the
   * parent size. If you are experiencing revalidation problems you can try
   * to set this constraint.
   */
  def noCache: this.type = { _lc.noCache; this }
  def isNoCache = _lc.isNoCache

  /**
   * If the whole layout should be non grid based. It is the same as setting
   * the "noGrid" property on every row/column in the grid.
   */
  def noGrid: this.type = { _lc.noGrid; this }
  def isNoGrid = _lc.isNoGrid

  /**
   * Turns off padding of visual bounds (e.g. compensation for drop shadows)
   * Defaults to true
   */
  def visualPadding(pad: Boolean): this.type = {
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
      add("grid gap x", _lc.getGridGapX).
      add("grid gap y", _lc.getGridGapY).
      add("height", _lc.getHeight).
      add("width", _lc.getWidth).
      add("hide mode", HideMode(_lc.getHideMode)).
      add("insets", _lc.getInsets).
      add("no cache", _lc.noCache).
      add("no grid", _lc.noGrid).
      add("visual padding", _lc.isVisualPadding).
      get
  }
}

object LC {
  def apply(): LC = new LC(new net.miginfocom.layout.LC())
}

/**
 *    AAA    CCCC
 *   A   A  C
 *   AAAAA  C      *************************************************************
 *   A   A  C
 *   A   A   CCCCC
 *
 * Column or row constraints.  Comments will pretend it is a column
 */
abstract class AC protected[smig] (ac: net.miginfocom.layout.AC) {
  protected val _ac = ac

  private[smig] def this() = this(new net.miginfocom.layout.AC())

  /**
   * For columns, the components in that column will default to a "growX"
   * constraint (which can be overridden by the individual component
   * constraints). Note that this property does not affect the size for the
   * row, but rather the sizes of the components in the row.
   */
  def fill: this.type = { _ac.fill; this }
  def fill(indexes: Int*): this.type = { _ac.fill(indexes: _*); this }

  /** Column width as a bound size */
  def size(bs: BS): this.type = { _ac.size(bs.toString); this }

  /** Several column widths as a bound size */
  def size(bs: BS, indexes: Int*): this.type = {
    _ac.size(bs.toString, indexes: _*)
    this
  }

  /**
   * Specifies that the current row/column should be put in the size group
   * and will thus share the same size constraints as the other components
   * in the group.
   * @param n Name of group
   */
  def sizeGroup(n: String): this.type = { _ac.sizeGroup(n); this }

  /**
   * Specifies that the given rows/columns should be put in the size group
   * and will thus share the same size constraints as the other components
   * in the group.
   * @param n Name of group
   */
  def sizeGroup(n: String, indexes: Int*): this.type = {
    _ac.sizeGroup(n, indexes: _*)
    this
  }

  def getSizeGroup(i: Int): Option[String] = {
    val grp = _ac.getConstaints()(i).getSizeGroup
    if (grp == null) None else Some(grp)
  }

  /** Moves to next column, setting gap */
  def gap(bs: BS): this.type = { _ac.gap(bs.toString); this }

  /** Sets gap after given indexes */
  def gap(bs: BS, indexes: Int*): this.type = {
    _ac.gap(bs.toString, indexes: _*); this
  }

  /**
   * Specifies the current row/column's grow weight within columns/rows
   * with the same grow priority
   */
  def grow(g: Float): this.type = { _ac.grow(g); this }
  def grow: this.type = grow(100.0F)
  def grow(g: Float, indexes: Int*): this.type = {
    _ac.grow(g, indexes: _*)
    this
  }

  /** Specifies the current row/column's shrink priority. */
  def growPrio(prio: Int): this.type = { _ac.growPrio(prio); this }
  def growPrio(prio: Int, indexes: Int*): this.type = {
    _ac.growPrio(prio, indexes: _*)
    this
  }

  /**
   * Specifies the current row/column's shrink weight within columns/rows
   * with the same shrink priority
   */
  def shrink(g: Float): this.type = { _ac.grow(g); this }
  def shrink: this.type = grow(100.0F)
  def shrink(g: Float, indexes: Int*): this.type = {
    _ac.shrink(g, indexes: _*)
    this
  }

  /** Specifies the indicated rows'/columns' shrink priority. */
  def shrinkPrio(prio: Int): this.type = { _ac.shrinkPrio(prio); this }
  def shrinkPrio(prio: Int, indexes: Int*): this.type = {
    _ac.shrinkPrio(prio, indexes: _*)
    this
  }

  /** Put whole column will be layed out in one cell. */
  def noGrid(): this.type = { _ac.noGrid; this }
  def noGrid(indexes: Int*): this.type = { _ac.noGrid(indexes: _*); this }

  /**
   * Set the index of the nect column to specify properties of, starts at
   * 0
   */
  def index(i: Int): this.type = {
    require(i >= 0)
    _ac.index(i)
    this
  }
  /** Alias for index */
  def i(i: Int): this.type = index(i)

  /** Total number of rows/columns */
  def count(i: Int): this.type = {
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
  def align(align: YPos): this.type = {
    require(align != null)
    _ac.align(align.toString.substring(1))
    this
  }
  def align(align: YPos, indexes: Int*): this.type = {
    require(align != null)
    _ac.align(align.toString.substring(1), indexes: _*)
    this
  }
}

object RowC {
  def apply(): RowC = new RowC()
}

/** Column constraint, create with ColC() */
class ColC private[smig] (ac: net.miginfocom.layout.AC) extends AC {
  private[smig] def this() = this(new net.miginfocom.layout.AC())

  /** Column's default alignment */
  def align(align: XPos): this.type = {
    require(align != null)
    _ac.align(align.toString.substring(1))
    this
  }
  def align(align: XPos, indexes: Int*): this.type = {
    require(align != null)
    _ac.align(align.toString.substring(1), indexes: _*)
    this
  }
}

object ColC {
  def apply(): ColC = new ColC()
}

/** Utility for printing */
private[smig] class Out(s: String) {
  private val _nl = System.getProperty("line.separator")
  private val _sb = new StringBuilder(s).append(_nl)
  private def add(field: String, enum: Enumeration): this.type =
    add(field, enum.toString.toLowerCase)

  private[smig] def add(field: String, value: Any): this.type = {
    if (value != null) {
      _sb.append("  ").append(field).append(" = ").append(value).append(_nl)
    }
    this
  }

  private[smig] def add(field: String, value: BoundSize): this.type = {
    if (value != null) {
      add(field, BS.toBS(value))
    }
    this
  }

  private[smig] def add(field: String, value: UnitValue): this.type = {
    if (value != null) {
      add(field, UV.toUV(value))
    }
    this
  }

  private[smig] def add(field: String, values: Array[UnitValue]): this.type = {
    if (values != null) {
      _sb.append("  ").append(field).append(" = [")
      values.map(UV.toUV(_).toString).addString(_sb, ", ")
      _sb.append(']').append(_nl)
    }
    this
  }

  def get = _sb.toString
}

/** 
 * If any callback is added for a component we maintain one of these in a map
 * for it. It holds any of the 3 methods that may be registered.
 */
private[smig] class MigCallback(comp: Component) {
  def component = comp
  /**
   * @return a [x, y, x2, y2] position similar to the "pos" in the component
   * constraint. If defined, any non-null ones override the ones specified on
   * the CC.
   */
  var position: (Component) => Option[(UV, UV, UV, UV)] = _
  /** @return a size similar to the "width" and "height" in the component 
   * constraint. */
  var size: (Component) => Option[(BS, BS)] = _
  /**
   * A last minute change of the bounds. The bound for the layout cycle has
   * been set and you can correct there using any set of rules you like.
   * @param screenBounds these are the bounds that will be used if we return
   * null
   * @return bounds to actually use
   */
  var correctBounds: (Component) => Option[(Int, Int, Int, Int)] = _

  private[smig] def getPosition: Array[UnitValue] =
    if (position == null) null else
      position(comp) match {
        case Some((uv1, uv2, uv3, uv4)) => Array(UV.toUnitValue(uv1),
          UV.toUnitValue(uv2), UV.toUnitValue(uv3), UV.toUnitValue(uv4))
        case _ => null
      }

  private[smig] def getSize: Array[BoundSize] =
    if (size == null) null else
      size(comp) match {
        case Some((bs1, bs2)) => Array(BS.toBoundSize(bs1), BS.toBoundSize(bs2))
        case _ => null
      }

  private[smig] def getCorrectBounds: (Int, Int, Int, Int) =
    if (correctBounds == null) null else {
      correctBounds(comp) match {
        case Some(t: (Int, Int, Int, Int)) => t
        case _ => null
      }
    }
}

private object SmigLayoutCallback extends LayoutCallback {
  private def migLayout(compWrapper: ComponentWrapper) = {
    val peer = compWrapper.getComponent.asInstanceOf[JComponent];
    MigPanel._callbacksByPeer.get(peer)
  }

  override def getPosition(compWrapper: ComponentWrapper): Array[UnitValue] = 
    migLayout(compWrapper) match {
      case Some(callback) => callback.getPosition
      case _ => null
    }

  override def getSize(compWrapper: ComponentWrapper): Array[BoundSize] = 
    migLayout(compWrapper) match {
      case Some(callback) => callback.getSize
      case _ => null
    }

  override def correctBounds(compWrapper: ComponentWrapper) {
    val callback = migLayout(compWrapper)
    callback match {
      case Some(callback) =>
        callback.getCorrectBounds match {
          case (i1, i2, i3, i4) => 
            callback.component.peer.setBounds(i1, i2, i3, i4)
          case _ =>
        }
      case _ =>
    }
  }
}

object MigPanel {
  private var _unique: Int = 0;

  /** Allow recovering Components based on the peer when doing callback */
  private[smig] lazy val _callbacksByPeer = 
    new WeakHashMap[JComponent, MigCallback]

  /**
   * May be used to generate unique group names.
   * Assume done in awt dispatch thread.
   */
  def uniqueGroupName: String = {
     _unique += 1
     "MigUtilBtnGroup_" + _unique
  }
  
  /**
   * May be used to generate unique ids.
   * Assume done in awt dispatch thread.
   */
  private[smig] def uniqueIdString: String = {
    _unique += 1
    "ID_" + _unique
  }

  /* MigLayout does not define constants for the hide mode parameters. */
  val HIDEMODE_NORMAL_SIZE = 0;
  val HIDEMODE_ZERO_SIZE = 1;
  val HIDEMODE_ZERO_SIZE_NO_GAP = 2;
  val HIDEMODE_NO_PARTICIPATION = 3;

  /**
   * Create a transparent {@code component} with huge maximum width.
   * Using it with fillX or fillY will make it take up space.
   *
   * Prefer addSpringX, etc, unless you are sharing constraints
   *
   * @return A normally transparent component.
   */
  def createSpring: Spring = new Spring()
  /** createSpring, only make visible */
  def createSpringDebug(bg: Color): Spring = new Spring().debug(bg)
}

/*******************************************************************************
 * A container configured with a MigLayout. You are so lucky.
 *
 * I decided the normal way of specifying position was slightly broken.
 * I ripped the positioning methods out of CC and put them in the
 * MigPanel.  The position is determined as you add components and is
 * always added to the constraints as a cell.  There is an origin, originally
 * defined as (0, 0).  There is a cursor "_pt" holding the current position.
 * The "put" methods add a component at the cursor.  The "add" methods  do the
 * same but increment the cursor.  The "dock" methods have been moved to the
 * MigPanel.
 ******************************************************************************/
class MigPanel private[this] (lc: Option[LC], rowC: Option[RowC],
  colC: Option[ColC])
  extends Panel with LayoutContainer {
  private var _flowX = true
  private var _xFlowRight = true
  private var _yFlowDown = true
  private var _pt = Array(0, 0)
  private var _origin = Array(0, 0)
  private var _wrapAfter = 0;
  private var _hasCallbacks = false
  private lazy val _lc: LC = lc match {
    case Some(lc) => lc
    case None => LC()
  }
  override lazy val peer =
    new JPanel(new MigLayout(
      _lc.java,
      rowC match { case None => null; case Some(rowC) => rowC.java },
      colC match { case None => null; case Some(colC) => colC.java })
  ) with SuperMixin
  private def mig = peer.getLayout.asInstanceOf[MigLayout]

  /** Basically you can just leave out any null args */
  def this(lc: LC, rowC: RowC, colC: ColC) =
    this(Some(lc), Some(rowC), Some(colC))
  def this(lc: LC, rowC: RowC) = this(Some(lc), Some(rowC), None)
  def this(lc: LC, colC: ColC) = this(Some(lc), None, Some(colC))
  def this(lc: LC) = this(Some(lc), None, None)
  def this(rowC: RowC, colC: ColC) = this(None, Some(rowC), Some(colC))
  def this(rowC: RowC) = this(None, Some(rowC), None)
  def this(colC: ColC) = this(None, None, Some(colC))
  def this() = this(None, None, None)

  /** Install component using copy of constraint */
  private def install(com: Component, con: CC): CC = {
    val cc = con.copy
    peer.add(com.peer, cc.java)
    cc
  }

  /** Install component, creating constraint */
  private def install(com: Component): CC = {
    val cc = new CC
    peer.add(com.peer, cc.java)
    cc
  }

  /**
   * This is an implementation of an  abstract method but we don't
   * really want to use it since it can't copy and return the CC.
   * We need a copy because the cell is explicitly set on each component.
   */
  @Deprecated
  override protected[smig] def add(com: Component, con: CC) {
    val cc = install(com, con)
    cc.java.cell(_pt(0), _pt(1))
    step
  }

  def add(con: CC, com: Component): CC = {
    val cc = install(com, con)
    cc.java.cell(_pt(0), _pt(1))
    step
    cc
  }

  /**
   * Adds and then steps to next cell.
   * @param com Add this
   * @return a freshly brewed CC for your modifying pleasure
   */
  def add(com: Component): CC = {
    val cc = put(com)
    step
    cc
  }

  /**
   * @param com Add this to current cell, don't step
   * @return a fresh CC for your modifying pleasure
   */
  def put(com: Component): CC = {
    val cc = install(com)
    cc.java.cell(_pt(0), _pt(1))
    cc
  }

  /**
   * @param com Add this to current cell, don't step
   * @param cc copy this and use it
   * @return the constraint copy
   */
  def put(cc: CC, com: Component): CC = {
    cc.java.cell(_pt(0), _pt(1))
    install(com, cc)
  }

  /** Set the grid pointer for the next insert */
  def goto(x: Int, y: Int): this.type = { _pt(0) = x; _pt(1) = y; this }
  /** Set the grid pointer for the next insert, changing only x. */
  def gotoX(x: Int): this.type = { _pt(0) = x; this }
  /** Set the grid pointer for the next insert, changing only y. */
  def gotoY(y: Int): this.type = { _pt(1) = y; this }
  /// @return the offsets of the current insert point */
  def getCell: (Int, Int) = (_pt(0), _pt(1))

  /** Set flow direction to X. */
  def flowX(): this.type = { _flowX = true; this }
  /** Set flow direction to Y. */
  def flowY(): this.type = { _flowX = false; this }
  /** @return true if flow direction is x */
  def isFlowX = _flowX
  /** @param b true if x flow direction should be to the right. */
  def xFlowRight(b: Boolean): this.type = { _xFlowRight = b; this }
  /** @return true if x flow direction is to right. */
  def isXFlowRight = _xFlowRight
  /** @param b true if y flow direction should be to the right. */
  def yFlowDown(b: Boolean): this.type = { _yFlowDown = b; this }
  /** @return true if y flow direction is down. */
  def isYFlowDown = _yFlowDown
  /** Sets and goes to origin */
  def origin(x: Int, y: Int): this.type = {
    _origin(0) = x;
    _origin(1) = y;
    toOrigin
  }
  /** Set pointer to origin */
  def toOrigin: this.type = {
    _pt(0) = _origin(0)
    _pt(1) = _origin(1)
    this
  }
  /** Get the amount to step in the x */
  def getXStep = (if (_xFlowRight) 1 else -1)
  /** Get the amount to step in the x */
  def getYStep = (if (_yFlowDown) 1 else -1)

  /**
   * If a step goes this far from origin in flow direction, increments
   * secondary direction and seeks back to origin in flow direction.
   */
  def wrapAfter(i: Int): this.type = {
    require(i >= 0)
    _wrapAfter = i
    this
  }

  /** increment 1 in flow direction */
  def step: this.type = step(1)

  /** increment however many in flow direction, wrapping if specified */
  def step(i: Int): this.type = {
    if (_flowX) {
      _pt(0) += getXStep * i
      if (_wrapAfter > 0) {
        if (_xFlowRight) {
          if (_pt(0) >= _origin(0) + _wrapAfter) newRow
        } else {
          if (_pt(0) <= _origin(0) - _wrapAfter) newRow
        }
      }
    } else {
      _pt(1) += getYStep * i
      if (_wrapAfter > 0) {
        if (_yFlowDown) {
          if (_pt(1) >= _origin(1) + _wrapAfter) newCol
        } else {
          if (_pt(1) <= _origin(1) - _wrapAfter) newCol
        }
      }
    }
    this
  }

  /** Set x to x of origin and step y in y flow direction */
  def newRow: this.type = {
    _pt(0) = _origin(0)
    _pt(1) += getYStep
    this
  }

  /** Set y to y of origin and step x in x flow direction */
  def newCol: this.type = {
    _pt(0) += getXStep
    _pt(1) = _origin(1);
    this
  }

  /**
   * For a component to take up the whole side, unless something
   * else is docked.  Operates independent of normal cell range
   * @return Fresh CC
   */
  def dock(dock: Dock, comp: Component): CC = {
    val cc = install(comp)
    val j = cc.java
    dock match {
      case North => j.dockNorth
      case West => j.dockWest
      case South => j.dockSouth
      case East => j.dockEast
      case _ =>
    }
    cc
  }

  /** Refresh interval */
  def getDebugMillis = _lc.java.getDebugMillis

  /**
   * Does the conventional debug, just allows calling it without defining
   * an LC in the constructor.  (One gets created regardless)
   */
  def debug(millis: Int): this.type = {
    _lc.java.debug(millis)
    this
  }

  /**
   * Does the conventional debug with default refresh, just allows calling
   * it without defining an LC in the constructor.  (One gets created
   * regardless)
   */
  def debug: this.type = debug(1000)

  /**
   * Arrange for useful tool tips telling the constraints are set on the
   * components
   * @return this
   */
  def debugTip: this.type = {
    ToolTipManager.sharedInstance().setDismissDelay(Int.MaxValue);
    val tip = new StringBuilder("<html>");
    str(_lc, tip)
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

  /** Debug tool tip stuff */
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
    row(tip, "NoCache", l.isNoCache)
    row(tip, "NoGrid", l.isNoGrid)
    row(tip, "TopToBottom", l.isTopToBottom)
    row(tip, "VisualPadding", l.isVisualPadding)
  }

  /** Debug tool tip stuff */
  private def str(ac: AC, tip: StringBuilder) {
    ac.java.getConstaints.iterator.zipWithIndex.foreach(pair => { 
      val c = pair._1  
      tip.append(pair._2).append(".<br>")
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

  /** Debug tool tip stuff */
  private def setCompTips(comp: Component) {
    val cc = mig.getConstraintMap.get(comp.peer).
      asInstanceOf[net.miginfocom.layout.CC]
    if (cc != null) {
      val horz = cc.getHorizontal
      val vert = cc.getVertical
      val dirs = Array(null, "North", "West", "South", "East")
      val tip = new StringBuilder("<html>CC = <br/>")
      row(tip, "AlignX", horz.getAlign)
      row(tip, "AlignY", vert.getAlign)
      row(tip, "Cell", new Dimension(cc.getCellX, cc.getCellY))
      row(tip, "Dock", dirs(cc.getDockSide + 1))
      row(tip, "GapBefore", horz.getGapBefore)
      row(tip, "GapAfter", horz.getGapAfter)
      row(tip, "GrowX", horz.getGrow)
      row(tip, "GrowY", vert.getGrow)
      row(tip, "GrowPrioX", horz.getGrowPriority)
      row(tip, "GrowPrioY", vert.getGrowPriority)
      row(tip, "Pad", str(cc.getPadding))
      row(tip, "PushX", cc.getPushX)
      row(tip, "PushY", cc.getPushY)
      row(tip, "SpanX", cc.getSpanX)
      row(tip, "SpanY", cc.getSpanY)
      row(tip, "Tag", cc.getTag)
      row(tip, "External", cc.isExternal)
      row(tip, "FlowX", cc.getFlowX)
      row(tip, "Name", comp.name)
      row(tip, "Size", comp.size)
      row(tip, "PreferredSize", comp.preferredSize)
      row(tip, "MinimumSize", comp.minimumSize)
      row(tip, "MaximumSize", comp.maximumSize)
      row(tip, "Background", comp.background)
      row(tip, "Foreground", comp.foreground)
      row(tip, "Opaque", comp.opaque)
      row(tip, "Visible", comp.visible)
      comp.tooltip = tip.append("</html>").toString
    }
  }

  /** Debug tool tip stuff */
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
    override def componentRemoved(ev: ContainerEvent) = {}
  }

  /** Debug tool tip stuff */
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
      } else if (value.isInstanceOf[Dimension]) {
        tip.append("&nbsp;").append(name).append(" = ").
          append(str(value.asInstanceOf[Dimension])).append("<br/>")
      } else {
        tip.append("&nbsp;").append(name).append(" = ").append(value).
          append("<br/>")
      }
    }
  }

  /** Tool tip printing */
  private def str(dim: Dimension): String = dim.width + " X " + dim.height

  /** Tool tip printing */
  private def str(pad: Array[UnitValue]): String = {
    if (pad == null) null else {
      val sb = new StringBuilder("[");
      pad.map(unitValue => {
        if (unitValue == null) "null" else UV.toUV(unitValue)
      }).addString(sb, ",")
      sb.append("]").toString
    }
  }

  /** Tool tip printing */
  private def str(bs: BoundSize): String = {
    if (bs == null) null else {
      val s = str(bs.getMin) + ":" + str(bs.getPreferred) + ":" +
        str(bs.getMax)
      if ("n:n:n" == s) null else s
    }
  }

  /** Tool tip printing */
  private def str(uv: UnitValue): String = {
    if (uv == null) "n" else {
      val str = uv.getConstraintString
      if (str == "()") "n" else str
    }
  }

  override protected def constraintsFor(comp: Component) = layout(comp)

  override protected def areValid(c: CC): (Boolean, String) =
    (true, "Like we really checked")

  /* @return Column constraints */
  def getColC: Option[ColC] = {
    val ac =
      mig.getColumnConstraints.asInstanceOf[net.miginfocom.layout.AC]
    if (ac == null) None else Some(new ColC(ac))
  }

  /* @return Row constraints */
  def getRowC: Option[RowC] = {
    val ac =
      mig.getRowConstraints.asInstanceOf[net.miginfocom.layout.AC]
    if (ac == null) None else Some(new RowC(ac))
  }

  /**
   * The Layout should already be on the correct cell, probably as the
   * result of a wrap
   * @param container add in this container which is using MigLayout
   * @param btns add this components, centered on row and with same width
   */
  def addBtnRow(sameSize: Boolean, btns: Component*): Unit =
    addBtnRow(-1, sameSize, btns: _*)

  /**
   * Flow in layout should be X
   * @param row Add on this row unless < 0
   * @param container add in this container which is using MigLayout
   * @param sameSize true to coerce buttons (or whatever) to same size
   * @param btns add these components, centered on row and with same width
   */
  def addBtnRow(row: Int, sameSize: Boolean, btns: Component*) {
    require(isFlowX)
    val spaceSizeGroup = MigPanel.uniqueGroupName
    if (row >= 0) {
      goto(0, row)
    }
    val bs0 = BS.toBS(0)
    put(MigPanel.createSpring).spanX.flowX.sizeGroupX(spaceSizeGroup).
      gapLeft(bs0).gapRight(bs0).gapTop(bs0).gapBottom(bs0).growX
    val cs = cc.sizeGroupX(spaceSizeGroup).
      gapLeft(bs0).gapRight(bs0).gapTop(bs0).gapBottom(bs0).fillX
    val cb = cc.gapLeft(bs0).gapRight(bs0).gapTop(bs0).gapBottom(bs0)
    if (sameSize) {
      cb.sizeGroupX(MigPanel.uniqueGroupName)
    }
    btns.foreach(comp => {
      put(cs, MigPanel.createSpring)
      put(cb, comp)
    })
    put(cs, MigPanel.createSpring)
    put(cs, MigPanel.createSpring)
  }

  /** Invisible component that pushes in X */
  def addXSpring: CC =
    add(MigPanel.createSpring).fillX.height(BS.toBS(0))
  def addXSpringDebug: CC = add(MigPanel.createSpringDebug(Color.red)).fillX.
    height(BS.toBS(Consts._NARROW))
  /** Invisible component that pushes in Y */
  def addYSpring: CC =
    add(MigPanel.createSpring).fillY.width(BS.toBS(0))
  def addYSpringDebug: CC = add(MigPanel.createSpringDebug(Color.red)).fillY.
    width(BS.toBS(Consts._NARROW))
  /** Invisible component that pushes in X and Y */
  def addXYSpring: CC = add(MigPanel.createSpring).fillX.fillY
  def addXYSpringDebug: CC =
    add(MigPanel.createSpringDebug(Color.red)).fillX.fillY
  /** Invisible component of given width */
  def addXStrut(len: Int): CC =
    add(MigPanel.createSpring).width(BS.toBS(len)).height(BS.toBS(0))
  def addXStrutDebug(len: Int): CC = add(MigPanel.createSpringDebug(
    Color.green)).width(BS.toBS(len)).height(BS.toBS(Consts._NARROW))
  /** Invisible component of given height */
  def addYStrut(len: Int): CC =
    add(MigPanel.createSpring).width(BS.toBS(0)).height(BS.toBS(len))
  def addYStrutDebug(len: Int): CC = add(MigPanel.createSpringDebug(
    Color.green)).width(BS.toBS(Consts._NARROW)).height(BS.toBS(len))
  /** Invisible component of given width, height */
  def addXYStrut(w: Int, h: Int): CC =
    add(MigPanel.createSpring).width(BS.toBS(w)).height(BS.toBS(h))
  def addXYStrutDebug(w: Int, h: Int): CC = add(MigPanel.createSpringDebug(
    Color.green)).width(BS.toBS(w)).height(BS.toBS(h))

  /**
   * Adds the callback functions that will be called at different stages of the
   * layout cycle.  Because the java LayoutCallback methods are called with the
   * peer we register each component individually and map peer to a MigCallback
   * that holds the Component, and 3 callback methods.  Since the components
   * are unique in the world, we use a weak global map.
   * @param callback Something overriding a method or more in Callback
   * @param comps Components to use the callback.
   */

  /** @return The callback info for a component, creating if necessary. */
  private[smig] def migCallback(component: Component): MigCallback = {
    val peer = component.peer
    MigPanel._callbacksByPeer.get(peer) match {
      case Some(callback) => callback
      case None =>
        val cb = new MigCallback(component)
        MigPanel._callbacksByPeer.put(peer, cb)
        cb
    }
  }

  /** 
   * Add callback function to return absolute position relative to
   * container. 
   */
  def addPositionCallback(callback: (Component) => Option[(UV, UV, UV, UV)],
    comps: Component*) {
    comps.foreach(comp => { migCallback(comp).position = callback })
    initCallbacks
  }

  /** Add callback function to return size. */
  def addSizeCallback(callback: (Component) => Option[(BS, BS)],
    comps: Component*) {
    comps.foreach(comp => { migCallback(comp).size = callback })
    initCallbacks
  }

  /**
   * Add callback function to return last minute change of the bounds. The bound
   * for the layout cycle has been set and you can correct there using
   * any set of rules you like.
   */
  def addCorrectBoundsCallback(callback: (Component) =>
    Option[(Int, Int, Int, Int)], comps: Component*) {
    comps.foreach(comp => { migCallback(comp).correctBounds = callback })
    initCallbacks
  }

  private def initCallbacks =
    if (!_hasCallbacks) {
      _hasCallbacks = true
      mig.addLayoutCallback(SmigLayoutCallback)
    }

  /** If several components want to use the same constraints. */
  def cc = new CC

  /**
   *   CCCC    CCCC
   *  C       C
   *  C       C      ***********************************************************
   *  C       C
   *   CCCCC   CCCCC
   *
   * Component constraints (Subclass of MigPanel you observe)
   */

  type CC = Constraints // more natural for mig users

  class Constraints private[smig] (cc: net.miginfocom.layout.CC) 
  extends Cloneable {
    private[smig] val _cc = cc

    def this() = this(new net.miginfocom.layout.CC())

    /** The java CC didn't implement Cloneable */
    private[smig] def copy: CC = {
      val cc = new net.miginfocom.layout.CC
      cc.setCellX(_cc.getCellX)
      cc.setCellY(_cc.getCellY)
      cc.setDockSide(_cc.getDockSide)
      cc.setFlowX(_cc.getFlowX)
      cc.setHideMode(_cc.getHideMode)
      cc.setHorizontal(_cc.getHorizontal)
      cc.setId(_cc.getId)
      cc.setNewlineGapSize(_cc.getNewlineGapSize)
      cc.setPadding(_cc.getPadding)
      cc.setPos(_cc.getPos)
      cc.setPushX(_cc.getPushX)
      cc.setPushY(_cc.getPushY)
      cc.setSkip(_cc.getSkip)
      cc.setSpanX(_cc.getSpanX)
      cc.setSpanY(_cc.getSpanY)
      cc.setSplit(_cc.getSplit)
      cc.setTag(_cc.getTag)
      cc.setVertical(_cc.getVertical)
      cc.setWrap(_cc.isWrap)
      cc.setWrapGapSize(_cc.getWrapGapSize)
      for (bool <- Array(true, false)) {
        val oldDc = _cc.getDimConstraint(bool)
        val newDc = cc.getDimConstraint(bool)
        newDc.setAlign(oldDc.getAlign)
        newDc.setEndGroup(oldDc.getEndGroup)
        newDc.setFill(oldDc.isFill)
        newDc.setGapAfter(oldDc.getGapAfter)
        newDc.setGapBefore(oldDc.getGapBefore)
        newDc.setGrow(oldDc.getGrow)
        newDc.setGrowPriority(oldDc.getGrowPriority)
        newDc.setNoGrid(oldDc.isNoGrid)
        newDc.setShrink(oldDc.getShrink)
        newDc.setShrinkPriority(oldDc.getShrinkPriority)
        newDc.setSize(oldDc.getSize)
        newDc.setSizeGroup(oldDc.getSizeGroup)
      }
      new CC(cc)
    }

    def alignX(a: AlignX): this.type = { _cc.alignX(a.toString); this }

    def alignY(a: AlignY): this.type = { _cc.alignY(a.toString); this }

    def align(aX: AlignX, aY: AlignY): this.type = {
      _cc.alignX(aX.toString)
      _cc.alignY(aY.toString)
      this
    }
    def getAlignX: AlignX = AlignX.toAlignX(_cc.getHorizontal.getAlign)
    def getAlignY: AlignY = AlignY.toAlignY(_cc.getVertical.getAlign)

    /** @return x coordinate */
    def getCellX: Int = _cc.getCellX

    /** @return y coordinate */
    def getCellY: Int = _cc.getCellY

    def getDock: Dock = Dock(_cc.getDockSide)

    /**
     * Specifies that the component should be put in the X end group "grp" and
     * will thus share the same ending coordinate as them within the group.
     * @return this
     */
    def endGroupX(grp: String): this.type = { _cc.endGroupX(grp); this }
    /**
     * Specifies that the component should be put in the Y end group grp and
     * will thus share the same ending coordinate as them within the group.
     * @return this
     */
    def endGroupY(grp: String): this.type = { _cc.endGroupY(grp); this }
    def getEndGroupX = _cc.getHorizontal.getEndGroup
    def getEndGroupY = _cc.getVertical.getEndGroup

    def external: this.type = { _cc.external; this }
    def isExternal: Boolean = _cc.isExternal

    /**
     * Convenience method for what is required to make something fill space in
     * the X direction.  It is shorthand for growX.pushX
     */
    def fillX: this.type = { _cc.growX; _cc.pushX; this }

    /**
     * Convenience method for what is required to make something fill space in
     * the X direction.  It is shorthand for growY.pushY
     */
    def fillY: this.type = { _cc.growY; _cc.pushY; this }

    /** Flow in X direction within cell */
    def flowX: this.type = { _cc.setFlowX(true); this }
    /** Flow in Y direction within cell */
    def flowY: this.type = { _cc.setFlowX(false); this }
    def isFlowX: Boolean = {
      _cc.getFlowX match {
        case null => true
        case _ => flowX.asInstanceOf[Boolean]
      }
    }

    /** Gap above a component */
    def gapTop(bs: BS): this.type = { _cc.gapTop(bs.toString); this }
    /** Gap left of a component */
    def gapLeft(bs: BS): this.type = { _cc.gapLeft(bs.toString); this }
    /** Gap below a component */
    def gapBottom(bs: BS): this.type = { _cc.gapBottom(bs.toString); this }
    def gapB(bs: BS) = gapBottom(bs)
    /** Gap right of a component */
    def gapRight(bs: BS): this.type = { _cc.gapRight(bs.toString); this }
    def gapX(left: BS, right: BS): this.type = {
      gapLeft(left)
      gapRight(right)
      this
    }
    def gapY(top: BS, bottom: BS): this.type = {
      gapTop(top);
      gapBottom(bottom);
      this
    }

    /** X growth weight for when several items in cell, sets to 100 */
    def growX: this.type = growX(100.0F)
    /** X growth weight for when several items in cell*/
    def growX(g: Float): this.type = {
      require(g >= 0.0F)
      _cc.growX(g)
      this
    }
    def getGrowX = _cc.getHorizontal.getGrow

    /** Y growth weight for when several items in cell, sets to 100 */
    def growY: this.type = growY(100.0F)
    /** Y growth weight for when several items in cell*/
    def growY(g: Float): this.type = {
      require(g >= 0.0F)
      _cc.growY(g)
      this
    }
    def getGrowY = _cc.getVertical.getGrow

    /** The X grow priority compared to other components in the same cell. */
    def growPrioX(i: Int): this.type = {
      require(i >= 0)
      _cc.growPrioX(i)
      this
    }
    def getGrowPrioX = _cc.getHorizontal.getGrowPriority

    /** The Y grow priority compared to other components in the same cell. */
    def growPrioY(i: Int): this.type = {
      require(i >= 0)
      _cc.growPrioY(i)
      this
    }
    def getGrowPrioY = _cc.getHorizontal.getGrowPriority

    /**
     * The minimum width for the component. The value will override any value
     * that is set on the component itself.
     */
    def minWidth(uv: UV): this.type = { _cc.minWidth(uv.toString); this }

    /** The width for the component as a bound size */
    def width(bs: BS): this.type = { _cc.width(bs.toString); this }
    def w(bs: BS) = width(bs)

    /**
     * The maximum width for the component. The value will override any value
     * that is set on the component itself.
     */
    def maxWidth(uv: UV): this.type = { _cc.maxWidth(uv.toString); this }

    /**
     * The minimum height for the component. The value will override any value
     * that is set on the component itself.
     */
    def minHeight(uv: UV): this.type = { _cc.minHeight(uv.toString); this }

    /** The height for the component as a bound size */
    def height(bs: BS): this.type = { _cc.height(bs.toString); this }
    def h(bs: BS) = height(bs)

    /**
     * The maximum height for the component. The value will override any value
     * that is set on the component itself.
     */
    def maxHeight(uv: UV): this.type = { _cc.maxHeight(uv.toString); this }

    def pad(top: UV, left: UV, bottom: UV, right: UV): this.type = {
      _cc.setPadding(Array[UnitValue](top.value, left.value,
        bottom.value, right.value))
      this
    }
    def pad(f: Float): this.type = {
      val uv = UV.toUV(f)
      pad(uv, uv, uv, uv);
      this
    }
    /** @return the absolute resizing in the last stage of the layout cycle. */
    def getPad(): Option[(UV, UV, UV, UV)] = {
      val pad = _cc.getPadding()
      if (pad == null) None else {
        Some((UV.toUV(pad(0)), UV.toUV(pad(1)),
          UV.toUV(pad(2)), UV.toUV(pad(3))))
      }
    }
    
    def pos(uvX1: UV, uvY1: UV): this.type = {
      _cc.pos(uvX1.toString, uvY1.toString)
      this
    }
    
    def pos(uvX1: Option[UV], uvY1: Option[UV], uvX2: Option[UV],
        uvY2: Option[UV]): this.type = {
      _cc.pos(
          uvX1 match { case Some(uv) => uv.toString; case _ => null },
          uvY1 match { case Some(uv) => uv.toString; case _ => null },
          uvX2 match { case Some(uv) => uv.toString; case _ => null },
          uvY2 match { case Some(uv) => uv.toString; case _ => null })
      this
    }

    /**
     * "pushX" indicates that the column that this component is in (the first
     * if the component spans several) should default to growing.  If any column
     * has been set to push, this value on the component does nothing as
     * column push takes precedence.
     */
    def pushX(weight: Float): this.type = { _cc.pushX(weight); this }
    def getPushX: Option[Float] = {
      val f = _cc.getPushX
      if (f == null) None else Some(f.asInstanceOf[Float])
    }

    /**
     * "pushY" indicates that the row that this component is in (the first
     * if the component spans) should default to growing.  If any row
     * has been set to push, this value on the component does nothing as
     * row push takes precedence.
     */
    def pushY(weight: Float): this.type = { _cc.pushY(weight); this }
    def getPushY: Option[Float] = {
      val f = _cc.getPushY
      if (f == null) None else Some(f.asInstanceOf[Float])
    }

    /** Same as pushX(100.0F) */
    def pushX: this.type = pushX(100.0F)
    /** Same as pushY(100.0F) */
    def pushY: this.type = pushY(100.0F)
    /** Same as pushX, pushY */
    def push: this.type = { _cc.pushX; _cc.pushY; this }

    /** Shrink weight for the component */
    def shrinkX(g: Float): this.type = {
      require(g >= 0.0F)
      _cc.shrinkX(g)
      this
    }
    def getShrinkX = _cc.getHorizontal.getShrink

    /** Shrink weight for the component */
    def shrinkY(g: Float): this.type = {
      require(g >= 0.0F)
      _cc.shrinkY(g)
      this
    }
    def getShrinkY = _cc.getVertical.getShrink

    /** The shrink priority compared to other components IN THE SAME CELL. */
    def shrinkPrioX(i: Int): this.type = {
      require(i >= 0)
      _cc.shrinkPrioX(i)
      this
    }
    def getShrinkPrioX = _cc.getHorizontal.getShrinkPriority

    /** The shrink priority compared to other components IN THE SAME CELL. */
    def shrinkPrioY(i: Int): this.type = {
      require(i >= 0)
      _cc.shrinkPrioY(i)
      this
    }
    def getShrinkPrioY = _cc.getVertical.getShrinkPriority

    /**
     * Specifies that the component should be put in the size group and will
     * thus share the size with others in the group.
     */
    def sizeGroupX(grp: String): this.type = { _cc.sizeGroupX(grp); this }
    def sizeGroupY(grp: String): this.type = { _cc.sizeGroupY(grp); this }
    def getSizeGroupX = _cc.getHorizontal.getSizeGroup
    def getSizeGroupY = _cc.getVertical.getSizeGroup

    /**
     * Span completely in X direction
     */
    def spanX: this.type = { _cc.spanX; this }
    /**
     * The number of cells the cell that this constraint's component
     * will span in the X dimension.
     */
    def spanX(w: Int): this.type = { _cc.spanX(w); this }
    def getSpanX: Int = _cc.getSpanY

    /**
     * Span completely in Y direction
     */
    def spanY: this.type = { _cc.spanY; this }
    /**
     * The number of cells the cell that this constraint's component
     * will span in the Y dimension.
     */
    def spanY(h: Int): this.type = { _cc.spanY(h); this }
    def getSpanY: Int = _cc.getSpanY

    def tag(t: Tag): this.type = { _cc.tag(t.toString); this }
    /**
     * @return The tag string
     */
    def getTag: Option[String] = {
      val tag = _cc.getTag
      if (tag == null) None else Some(tag)
    }
    
    /** Set an id and return an object that may be used to perform 
     * calculations using it.  You can put this on the end of a CC 
     * expression and catch the value. */
    def id: ID = {
      val id = MigPanel.uniqueIdString
      _cc.setId(id)
      new ID(id)
    }

    /** The java peer */
    def java = _cc
  }
}

private[smig] object Consts {
  private[smig] val _NARROW = 3
}
/**
 * An invisible component unless set to debug
 * Use MigPanel.addXSpring, etc.
 */
class Spring private[smig] () extends Component {
  override lazy val peer: JComponent = new JLabel() with SuperMixin

  /** Add name, width, color */
  private[smig] def debug(bg: Color): Spring = {
    name = "Debugged Spring"
    opaque = true
    background = new Color((bg.getRGB() & (~0 >> 8)) | (0x80 << 24))
    this
  }
}
