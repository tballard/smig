/*
 * srcbin
 */
package java_test;

import javax.swing.BorderFactory;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;
import scala.swing.Frame;
import scala.swing.Label;
import scala.swing.MainFrame;
import scala.swing.SimpleSwingApplication;
import scala.swing.Swing.*;
import smig.*;
import smig.AlignX.*;
import smig.ColC;
import smig.RowC;
import smig.RowC.*;
import smig.Dock;
import smig.Dock.*;
import smig.XPos;
import smig.XPos.*;
import smig.YPos.*;

/**
 *
 * @author tballard
 */
public class SmigDemo extends SimpleSwingApplication {
   @Override
   public Frame top() {
      return new SmigFrame();
   }

   private class SmigFrame extends MainFrame {
      private SmigFrame() {
         title_$eq("Baby App");
         contents_$eq(new TopPanel());
      }
   }

   private class TopPanel extends MigPanel {
      private TopPanel() {
         super(LC.apply().fillX().insets(PX.apply(2)),
            ((RowC) RowC.apply().align(YPos.Ytop()).grow(6.0F).
            i(1)).align(YPos.Ybottom()),
            ColC.apply().align(XPos.Xleft()));
         debugTip();
         debug();
         dock(Dock.North(), new GrowDemo());
      }
   }

   private class GrowDemo extends MigPanel {
      private GrowDemo() {
         border_$eq(titled("Grow: priority-grow"));
         put(new BLbl("1-100")).width(BS.toBS(100)).maxWidth(UV.toUV(200)).
            growX(100).growPrioX(1).pushX();
         put(new BLbl("2-100")).width(BS.toBS(100)).maxWidth(UV.toUV(200)).
            growX(100).growPrioX(2);
         put(new BLbl("1-20")).width(BS.toBS(100)).maxWidth(UV.toUV(200)).
            growX(20).growPrioX(1);
         put(new BLbl("2-20")).width(BS.toBS(100)).maxWidth(UV.toUV(200)).
            growX(20).growPrioX(2);
      }
   }

   private Border titled(String title) {
      return BorderFactory.createTitledBorder(bord(), title);
   }

   private Border bord() {
      return BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
   }

   class BLbl extends Label {
      private BLbl(String text) {
         super(text);
         border_$eq(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
      }
   }

   class TBLbl extends Label {
      private TBLbl(String text, String title) {
         super(text);
         border_$eq(titled(title));
      }
   }
}
