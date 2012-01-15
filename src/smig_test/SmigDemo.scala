/*
 * srcbin
 */

package mig_test

import java.awt.Color._
import java.awt.{
  Dimension,
  Font
}
import java.awt.Font._
import scala.swing.{
  Button,
  Component,
  Label,
  MainFrame,
  Panel,
  SimpleSwingApplication,
  Swing
}
import scala.swing.Swing._
import scala.util.Random
import smig.{AlignX, BS, MigPanel, LC, RowC, ColC, PX, PCT, UV }
import smig.AlignX._
import smig.Dock._
import smig.XPos._
import smig.YPos._

object MigDemo extends SimpleSwingApplication {
  override def top = new MainFrame {
    title = "Baby App"
    contents = new MigPanel (
      // Create LC, set some params, note insets convert int to PX
      LC().fillX.insets(2),
      // Note we used derived classes of AC for a bit better checking
      RowC().grow(6.0F).align(Ytop).i(1).align(Ybottom),
      ColC().align(Xleft)
    ){
      // Add tool tips that show constraints
      debugTip
      // Add conventional debug even without LC in constructor
      debug
      dock(North, fillPanels)
      put(wrapDemo); put(centeredDemo).fillX.fillY
      newRow.put(shrinkDemo)
      newRow.put(growDemo)
      newRow.put(gapDemo).pushX.growX
      newRow.put(buttonsDemo).spanX.fillX
      newRow.put(springDemo).height(130).fillX
      newRow.put(endGroupDemo); put(randPanel)
      dock(East, eastPanel)
      dock(West, new TBLbl("California?", "West"))
      dock(South, sizeGroupDemo)   
    }
  }
  
  private def eastPanel = new MigPanel (
    LC().bottomToTop(true),
    RowC().align(Ybottom),
    ColC().i(0).align(Xright).i(1).align(Xleft).fill
  ){
    flowY.debugTip
    border = TitledBorder(EtchedBorder(Raised), "Up")
    "Uh, ok like I'm sure.".split(" ").foreach(str => {add(new BLbl(str))})
  }
  
  private def randPanel = new MigPanel (){
    debugTip
    border = titled("Random Gap Demo")
    for { i <- 0 to 5 } add(block).sizeGroupX("blockx").sizeGroupY("blocky")
  
    private def block = new MigPanel(
      LC().insets(PX(0))
    ){
      debugTip
      def rand = new Random
      border = TitledBorder(EtchedBorder(Raised), "Rand")
      val top : Int = rand.nextInt(20)
      val left : Int = rand.nextInt(20)
      val bottom = 19 - top
      val right = 19 - left
      add(new Label("uh"){
          border = TitledBorder(EtchedBorder(Lowered), "Uh!")
        }).gapTop(top).gapLeft(left).
      gapBottom(bottom).gapRight(right)
    }
  }
  
  private def fillPanels = new MigPanel (
    LC().gap(0, 0)
  ){
    flowY.debugTip
    border = titled("No fill or grow here")
    // Note how add or put returns a CC for chaining
    add(fillPanel("just pushing").debugTip).pushX
    add(fillPanel("I'm growing only because he's pushing -- weird").debugTip).
    growX
  }
  
  /** A row to fill. */
  private def fillPanel(lbl: String) = new MigPanel (
    LC().gap(1, 1),
    ColC().fill(2)
  ){
    flowX.debugTip
    border = titled(lbl)
    for (i <- 0 to 3) {
      val grow = (i % 2) == 0
      val push = i < 2
      val label = new Label("Btn") {
        border = TitledBorder(EtchedBorder(Lowered), 
                              (if (grow) "gr" else "**") + 
                              (if (push) "-pu" else "-**"))
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
  
  private def shrinkDemo = new MigPanel(
  ){
    border = titled("Shrink: priority-shrink")
    put(new BLbl("1-100")).width(100).shrinkX(100).shrinkPrioX(1)
    put(new BLbl("2-100")).width(100).shrinkX(100).shrinkPrioX(2)
    put(new BLbl("1-20")).width(100).shrinkX(20).shrinkPrioX(1)
    put(new BLbl("2-20")).width(100).shrinkX(20).shrinkPrioX(2)
  }
  
  private def growDemo = new MigPanel(
  ){
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
  
  private def wrapDemo = new MigPanel(){
    override def add(comp: Component) : CC = { 
      val cc = super.add(comp)
      if (getCell._1 == 4) newRow
      cc
    }
    border = titled("Wrap after 4")
    "Zero One Two Three Four Five Six Seven".split(" ").
    foreach(str => {add(new Label(str))})
  }
  
  private def centeredDemo = new MigPanel() {
    border = titled("Centering")
    add(new BLbl("Center me, baby") {
      }).alignX(CENTER).pushX
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
  
  private def endGroupDemo = new MigPanel(
  ) {
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
}
