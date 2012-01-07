/*
 * srcbin
 */

package mig_test

import java.awt.Color._
import java.awt.Font
import java.awt.Font._
import scala.swing.Label
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.Swing
import scala.swing.Swing._
import scala.util.Random
import smig.LC
import smig.MigPanel
import smig.ColC
import smig.PCT
import smig.PX
import smig.RowC
import smig.Dock._
import smig.XPos._
import smig.YPos._

object MigDemo extends SimpleSwingApplication {
  override def top = new MainFrame {
    title = "Baby App"
    contents = new MigPanel (
      LC().fillX.flowX.insets(2),
      RowC().grow(6.0F).align(Ytop).i(1).align(Ybottom),
      ColC().align(Xleft)
    ){
      debugTip
      add(new Label("NewsGator Account") {             
          font = font.deriveFont(BOLD)
          background = green
          border = TitledBorder(EtchedBorder, "One") 
        }).cell(0, 0)
      add(new Label("Other Account")).cell(1, 0)
      add(new Label("<html>thing-a<br>ma-jig</html>") {             
          font = font.deriveFont(ITALIC)
          foreground = blue
          border = TitledBorder(BeveledBorder(Lowered), "Two") 
        }).cell(0, 1).spanX(2).alignX(Xcenter)
      add(new Label("thang") {             
          font = font.deriveFont(ITALIC)
          border = TitledBorder(BeveledBorder(Raised), "Three") 
        }).cell(0, 2).spanX(2).alignX(PCT(25))
      add(new Label("thyng") {             
          font = font.deriveFont(ITALIC)
          border = TitledBorder(BeveledBorder(Raised), "Four") 
        }).cell(0, 2).spanX(2).alignX(PCT(75))
      add(new Label("thang") {             
          font = font.deriveFont(ITALIC)
          border = TitledBorder(BeveledBorder(Raised), "Five") 
        }).cell(0, 2).spanX(2).alignX(PCT(45))
      add(eastPanel).dock(East)
      add(new Label("west") {             
          font = font.deriveFont(ITALIC).deriveFont(BOLD)
          border = TitledBorder(BeveledBorder(Raised), "Six") 
        }).dock(West)
      add(fillPanels).dock(North)
      add(randPanel).dock(South)     
      add(new Label("<html>one<br>ton</html>") { 
          border = TitledBorder(BeveledBorder(Raised), "Seven") 
        }).cell(2,0).align(0, 0)
      add(new Label("twentyfive") {
          border = TitledBorder(BeveledBorder(Raised), "Eight")
        }).cell(2,1).align(Xcenter, Ycenter)
      add(new Label("3") {
          border = TitledBorder(BeveledBorder(Raised), "Nine")
        }).cell(2,2).align(Xright, Ybottom)
    }
  }
  
  def eastPanel = new MigPanel (
    LC().flowX,
    RowC().align(Ybottom),
    ColC().i(0).align(Xright).i(1).align(Xleft).fill
  ){
    debugTip
    border = TitledBorder(EtchedBorder(Raised), "Ten")
    add(new Label("uh"){
        border = TitledBorder(EtchedBorder(Lowered), "Eleven")
      }).pad(4)
    add(new Label("ok"){
        border = TitledBorder(EtchedBorder(Lowered), "Twelve")
      }).wrap
    add(new Label("like"){
        TitledBorder(LineBorder(CYAN, 4), "Thirteen")
      })
    add(new Label("sure"))
  }
  
  def randPanel = new MigPanel (
    LC().flowX
  ){
    debugTip
    border = TitledBorder(EtchedBorder(Lowered), "Fourteen")
    for { i <- 0 to 8 } add(block).sizeGroupX("blockx").sizeGroupY("blocky")
  }
  
  def block = new MigPanel(
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
  
  def fillPanels = new MigPanel (
    LC().flowY.gap(0, 0)
  ){
    debugTip
    border = TitledBorder(EtchedBorder(Lowered), "No fill or grow here")
    add(fillPanel("just pushing").debugTip).pushX
    add(fillPanel("I'm growing only because he's pushing").debugTip).growX
  }
  
  /** A row to fill. */
  def fillPanel(lbl: String) = new MigPanel (
    LC().flowX.gap(1, 1),
    ColC().fill(2)
  ){
    debugTip
    border = TitledBorder(EtchedBorder(Lowered), lbl)
    for (i <- 0 to 3) {
      val grow = (i % 2) == 0
      val push = i < 2
      val label = new Label("Btn") {
        border = TitledBorder(EtchedBorder(Lowered), 
                              (if (grow) "gr" else "**") + 
                              (if (push) "-pu" else "-**"))
        font = Font.decode(MONOSPACED)
      }
      val cc = add(label).cellX(i)
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
}


