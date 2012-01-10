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
import smig.AlignX
import smig.AlignX._
import smig.{MigPanel, LC, RowC, ColC, PX, PCT }
import smig.Dock._
import smig.XPos._
import smig.YPos._
import tom.bug.Debug

object MigDemo extends SimpleSwingApplication {
  override def top = new MainFrame {
    title = "Baby App"
    contents = new MigPanel (
      // Create LC, set some params, note insets convert int to PX
      LC().fillX.flowX.insets(2).debug(),
      // Note we used derived classes of AC for a bit better checking
      RowC().grow(6.0F).align(Ytop).i(1).align(Ybottom),
      ColC().align(Xleft)
    ){
      println("Top 2")
      // Add tool tips that show constraints
      debugTip
      // Add conventional debug even without LC in constructor
      debug(500)
      add(wrapDemo)
      add(shrinkDemo).newline
      add(growDemo).spanX.pushX.growX.newline
      add(centeredDemo).spanX.pushX.growX.newline.wrap
      add(new TBLbl("New Skater Account", "One") {             
          font = font.deriveFont(BOLD)
          background = green
        }).cell(0, 0)
      // Note that add method returns a CC for chaining
      add(new Label("Other Account")).cell(1, 0)
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
    }
  }
  
  def eastPanel = new MigPanel (
    LC().flowX.debug,
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
    LC().flowX.debug
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
    border = titled("No fill or grow here")
    add(fillPanel("just pushing").debugTip).pushX
    add(fillPanel("I'm growing only because he's pushing -- weird").debugTip).
    growX
  }
  
  /** A row to fill. */
  def fillPanel(lbl: String) = new MigPanel (
    LC().flowX.gap(1, 1),
    ColC().fill(2)
  ){
    debugTip
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
  
  def shrinkDemo = new MigPanel(
  ){
    border = titled("Shrink: priority-shrink")
    add(new BLbl("1-100")).width(100).shrinkX(100).shrinkPrioX(1).cell(0, 0)
    add(new BLbl("2-100")).width(100).shrinkX(100).shrinkPrioX(2).cell(0, 0)
    add(new BLbl("1-20")).width(100).shrinkX(20).shrinkPrioX(1).cell(0, 0)
    add(new BLbl("2-20")).width(100).shrinkX(20).shrinkPrioX(2).cell(0, 0)
  }
  
  def growDemo = new MigPanel(
  ){
    border = titled("Grow: priority-grow")
    add(new BLbl("1-100")).width(100).maxWidth(200).
    growX(100).growPrioX(1).cell(0, 0).pushX
    add(new BLbl("2-100")).width(100).maxWidth(200).
    growX(100).growPrioX(2).cell(0, 0)
    add(new BLbl("1-20")).width(100).maxWidth(200).
    growX(20).growPrioX(1).cell(0, 0)
    add(new BLbl("2-20")).width(100).maxWidth(200).
    growX(20).growPrioX(2).cell(0, 0)
  }
  
  def wrapDemo = new MigPanel(
    LC().wrapAfter(4)
  ){
    require(getLC.getWrapAfter == 4)
    border = titled("Wrap after 4")
    add(new Label("Zero"))
    add(new Label("One"))
    add(new Label("Two"))
    add(new Label("Three"))
    add(new Label("Four"))
    add(new Label("Five"))
    add(new Label("Six"))
  }
  
  def centeredDemo = new MigPanel() {
    border = titled("Centering")
    add(new BLbl("Center me, baby") {
      }).alignX(CENTER).pushX
}

  def sizeGroupDemo = new MigPanel() {
    border = titled("Size Group")
    
  }
  
  def titled(title: String) = { 
    TitledBorder(bord, title)
  }
  
  def bord = EtchedBorder(Lowered)
  
  class BLbl(text: String) extends Label(text) {
    border = bord
  }
  
  class TBLbl(text: String, title: String) extends Label(text) {
    border = titled(title)
  }
  
  
}

