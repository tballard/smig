/*
 * srcbin
 */

package smig_demo

import java.awt.Color._
import java.awt.Insets
import java.awt.{ Dimension, Font, Point, Color, Graphics, Graphics2D, GradientPaint }
import java.awt.Font._
import javax.swing.SwingUtilities
import scala.collection.mutable.HashMap
import scala.swing.event.{ ButtonClicked, MouseMoved }
import scala.swing.{ Button, Component, Container, Label, MainFrame, Panel, Publisher, SimpleSwingApplication, Swing }
import javax.swing.{ JComponent, JPanel, Timer }
import net.miginfocom.layout.{ BoundSize, ComponentWrapper, UnitValue }
import scala.swing.Swing._
import scala.util.Random
import smig.{ AlignX, AlignY, BS, LC, MigCallback, MigPanel, RowC, ColC, PX, PCT, UV }
import smig.AlignX._
import smig.AlignY._
import smig.Dock._
import smig.XPos._
import smig.YPos._
import java.awt.event.{ ActionEvent, ActionListener }
import net.miginfocom.layout.LayoutCallback
import scala.swing.event.MouseExited
import scala.swing.event.MouseEntered

object SmigDemo extends SimpleSwingApplication {
  override def top = new MainFrame {
    title = "Baby App"
    contents = new MigPanel(
      // Create LC, set some params, note insets convert int to PX
      LC().fillX.insets(2),
      // Note we used derived classes of AC for a bit better checking
      RowC().grow(6.0F).align(Ytop).i(1).align(Ybottom),
      ColC().align(Xleft)) {
      // Add tool tips that show constraints
      debugTip
      // Add conventional debug even without LC in constructor
      debug
      dock(North, fillPanels)
      put(wrapDemo); put(centeredDemo).fillX.fillY
      newRow.put(shrinkDemo).fillX; addXStrut(450)
      newRow.put(growDemo).fillX
      newRow.put(gapDemo).fillX
      newRow.put(buttonsDemo).spanX.fillX
      newRow.put(springDemo).height(130).fillX
      newRow.put(endGroupDemo); put(randPanel)
      val demo = callbackDemo
      newRow.put(demo)

      dock(East, eastPanel)
      dock(West, placementDemo)
      dock(South, sizeGroupDemo)
    }
  }

  private def placementDemo = new MigPanel() {
    border = titled("West")
    flowY.wrapAfter(6)
    (1 to 9).foreach(num => { add(new BLbl(num.toString)) })
    origin(1, 10).yFlowDown(false).xFlowRight(false)
    (1 to 9).foreach(num => { add(new BLbl(num.toString)) })
  }

  private def eastPanel = new MigPanel(
    LC().bottomToTop(true),
    RowC().align(Ybottom),
    ColC().i(0).align(Xright).i(1).align(Xleft).fill) {
    border = titled("Up")
    flowY.debugTip
    "Pour in from bottom? Uh, ok like I'm sure.".split(" ").
      foreach(str => { add(new BLbl(str)) })
  }

  private def randPanel = new MigPanel() {
    debugTip
    border = titled("Random Gap Demo")
    for { i <- 0 to 5 } add(block).sizeGroupX("blockx").sizeGroupY("blocky")

    private def block = new MigPanel(
      LC().insets(PX(0))) {
      debugTip
      def rand = new Random
      border = titled("Rand")
      val top: Int = rand.nextInt(20)
      val left: Int = rand.nextInt(20)
      val bottom = 19 - top
      val right = 19 - left
      add(new TBLbl("uh", "Uh!")).gapTop(top).gapLeft(left).
        gapBottom(bottom).gapRight(right)
    }
  }

  private def fillPanels = new MigPanel(
    LC().gap(0, 0)) {
    flowY.debugTip
    border = titled("No fill or grow here")
    // Note how add or put returns a CC for chaining
    add(fillPanel("just pushing").debugTip).pushX
    add(fillPanel("I'm growing only because he's pushing -- weird").debugTip).
      growX
  }

  /** A row to fill. */
  private def fillPanel(lbl: String) = new MigPanel(
    LC().gap(1, 1),
    ColC().fill(2)) {
    flowX.debugTip
    border = titled(lbl)
    for (i <- 0 to 3) {
      val grow = (i % 2) == 0
      val push = i < 2
      val label = new TBLbl("Btn", (if (grow) "gr" else "**") +
        (if (push) "-pu" else "-**")) {
        font = Font.decode(MONOSPACED)
      }
      val cc = add(label)
      var strs = List[String]()
      if ((i % 2) == 0) {
        cc.growX
        strs = "-Grow-" :: strs
      }
      if (i < 2) {
        cc.pushX
        strs = "-Push-" :: strs
      }
      label.text = "<html>XXXX<br/>" + strs.mkString("<br/>") + "</html>"
    }
  }

  private def shrinkDemo = new MigPanel() {
    border = titled("Shrink: priority-shrink")
    put(new BLbl("1-100")).width(100).shrinkX(100).shrinkPrioX(1)
    put(new BLbl("2-100")).width(100).shrinkX(100).shrinkPrioX(2)
    put(new BLbl("1-20")).width(100).shrinkX(20).shrinkPrioX(1)
    put(new BLbl("2-20")).width(100).shrinkX(20).shrinkPrioX(2)
  }

  private def growDemo = new MigPanel() {
    border = titled("Grow: priority-grow")
    put(new BLbl("1-100")).width(100).maxWidth(200).
      growX(100).growPrioX(1).pushX
    put(new BLbl("2-100")).width(100).maxWidth(200).
      growX(100).growPrioX(2)
    put(new BLbl("1-20")).width(100).maxWidth(200).
      growX(20).growPrioX(1)
    put(new BLbl("2-20")).width(100).maxWidth(200).
      growX(20).growPrioX(2)
  }

  private def wrapDemo = new MigPanel() {
    wrapAfter(4)
    border = titled("Wrap after 4")
    "Zero One Two Three Four Five Six Seven".split(" ").
      foreach(str => { add(new Label(str)) })
  }

  private def centeredDemo = new MigPanel() {
    border = titled("Centering")
    add(new BLbl("Center me, baby") {
    }).alignX(AlignX.CENTER).pushX
  }

  private def gapDemo = new MigPanel() {
    debug
    border = titled("Gaps")
    val bs = BS(5, 10, UV.INF)
    val zero = BS(0, 0, 0)
    add(new BLbl("Gap me all around, baby") {
    }).gapLeft(bs).gapRight(bs).gapTop(bs).gapBottom(bs)
    add(new BLbl("I got no gaps specified!") {
    })
    add(new BLbl("I got all gap 0") {
    }).gapLeft(zero).gapRight(zero).gapTop(zero).gapBottom(zero)
  }

  private def sizeGroupDemo = new MigPanel() {
    border = titled("Even/Odd size groups")
    add(new BLbl("0")).sizeGroupX("even")
    add(new BLbl("1")).sizeGroupX("odd")
    add(new BLbl("10")).sizeGroupX("even")
    add(new BLbl("11")).sizeGroupX("odd")
    add(new BLbl("100")).sizeGroupX("even")
    add(new BLbl("101")).sizeGroupX("odd")
    add(new BLbl("1000")).sizeGroupX("even")
    add(new BLbl("103")).sizeGroupX("odd")
    add(new BLbl("100000")).sizeGroupX("even")
  }

  private def springDemo = new MigPanel() {
    border = titled("Spring/Strut Demo")
    debugTip
    val dim = new Dimension(20, 20)
    for (x <- 0 to 6) {
      for (y <- 0 to 4) {
        val xOdd = x % 2 == 1
        val yOdd = y % 2 == 1
        (if (xOdd && yOdd) {
          goto(x, y)
          add(new Panel() { preferredSize = dim; background = blue })
        } else if (xOdd) {
          if (x == 1) {
            if (y == 2) {
              goto(x, y).addYStrutDebug(5)
            } else {
              goto(x, y).addYSpringDebug
            }
          }
        } else if (yOdd) {
          if (y == 1) {
            if (x == 2) {
              goto(x, y).addXStrutDebug(30)
            } else {
              goto(x, y).addXSpringDebug
            }
          }
        })
      }
    }
  }

  private def endGroupDemo = new MigPanel() {
    border = titled("EndGroup: 'X' works, '?' not so much")
    put(new BLbl("0"))
    put(new BLbl("?")).endGroupX("one")
    put(new BLbl("0"))
    put(new BLbl("X")).endGroupX("two")
    put(new BLbl("000000000000000000000000000000"))
    goto(0, 1).put(new BLbl("?????")).endGroupX("one")
    put(new BLbl("XX")).endGroupX("two")
    goto(0, 2).add(new BLbl("XX")).endGroupX("two")
  }

  private def buttonsDemo = new MigPanel() {
    border = titled("Button Demo")
    addBtnRow(true, new Button("Exit"), new Button("Bail"), new Button("Quit"))
  }

  private def titled(title: String) = {
    TitledBorder(bord, title)
  }

  private def bord = EtchedBorder(Lowered)

  private class BLbl(text: String) extends Label(text) {
    border = bord
  }

  private class TBLbl(text: String, title: String) extends Label(text) {
    border = titled(title)
  }

  /** This is a rewrite of the callback demo that comes with Mig */
  private def callbackDemo = new MigPanel(
    LC().alignX(AlignX.CENTER).alignY(AlignY.BOTTOM)) with ActionListener {
    border = titled("MiG Layout Callback Demo - Click a button")

    private val _repaintTimer = new Timer(100, this)
    private val _pressMap = new HashMap[Component, Long]
    private var _mousePos: Point = _

    val btns = (0 until 10).map(num => {
      val btn = createBtn(num)
      add(btn)
      btn
    })

    // This is the size change part
    val resize = (comp: Component) => Option[(BS, BS)] {
      comp match {
        case (b: Button) =>
          val p: Point = if (_mousePos == null) new Point(-1000, -1000) else
            SwingUtilities.convertPoint(peer, _mousePos, b.peer)
          val fac = Math.sqrt(Math.pow(
            Math.abs(p.x - b.size.width / 2f), 2) +
            Math.pow(Math.abs(p.y - b.size.height / 2f), 2))
          val fact = Math.max(2 - (fac / 200), 1).toFloat
          val uv: UV = UV.toUV(new UnitValue(70 * fact))
          val bs: BS = BS(uv)
          (bs, bs)
        case _ => null
      }
    }

    // This is the bouncing part
    val rebound = (c: Component) => Option[(Int, Int, Int, Int)] {
      _pressMap.get(c) match {
        case Some(l) =>
          val duration = System.currentTimeMillis - l
          val origHeight = 20.0
          val diminished = duration * 0.005
          val maxHeight = origHeight - diminished
          val deltaY = math.round(math.abs(math.sin(duration / 300.0) *
            maxHeight)).intValue
          val p = c.peer
          if (maxHeight < 0.5) {
            _pressMap.remove(c)
            if (_pressMap.isEmpty) _repaintTimer.stop()
          }
          (p.getX, p.getY - deltaY, p.getWidth, p.getHeight())
        case None => null
      }
    }
    addCorrectBoundsCallback(rebound, btns: _*)
    addSizeCallback(resize, btns: _*)

    listenTo(this)

    override def actionPerformed(ev: ActionEvent) = {
      revalidate
    }

    override def paintComponent(g: Graphics2D) {
      g.setPaint(
        new GradientPaint(0, size.height / 2, Color.WHITE, 0,
          size.height, new Color(240, 238, 235)))
      g.fillRect(0, 0, size.width, size.height)
    }

    def createBtn(i: Int): Button = {
      val ch: String = "MIG LAYOUT".charAt(i).toString
      val button: Button = new Button(ch) {
        focusPainted = false
        margin = new Insets(0, 0, 0, 0)
      }
      listenTo(button)
      listenTo(mouse.moves)
      button
    }
    listenTo(mouse.moves)

    reactions += {
      case MouseMoved(comp, pt, modifiers) =>
        if (comp.isInstanceOf[Button]) {
          _mousePos = SwingUtilities.convertPoint(
            comp.peer, pt, peer)
        } else {
          _mousePos = pt
        }
        revalidate
      case MouseEntered(comp, pt, modifiers) =>
        _mousePos = null;
        revalidate
      case ButtonClicked(button) =>
        _pressMap.put(button.asInstanceOf[Component], System.currentTimeMillis());
        _repaintTimer.start();
    }
  }
}



