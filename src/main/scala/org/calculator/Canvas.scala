package org.calculator

import java.awt.{BasicStroke, Color, Graphics2D, Stroke}
import scala.swing.{Dimension, Panel}
import scala.util.control.Breaks.break

class Canvas extends Panel {
  var p: TreeNode2 = TreeNode2("0")

  override def paintComponent(g: Graphics2D): Unit ={
    var t = new Transform(200, 200, 80) // Should be an element property

    g.clearRect(0, 0, size.width, size.height)
    /*
    g.setColor(Color.blue)
    g.fillOval(0, 0, 100, 100)
     */


    // Tracer les grilles
    // Doit trouver un système de test pour automatiser la création des grilles
    // Une matrice de transforamtion devrait aussi être utilisé pour la conversion des coordonnées
    g.setPaint(Color.lightGray)
    g.drawLine(200, 0, 200, 400)
    for(x <- -4 to 4){ // Vertical
      var _x = t.apply(x, 0)._1
      g.drawLine(_x.toInt, 0, _x.toInt, 400)
    }
    for(y <- -4 to 4){ // HOrizontal
      var _y = t.apply(0, y)._2
      g.drawLine(0, _y.toInt, 400, _y.toInt)
    }


    // X Axis and Y axis
    g.setPaint(Color.BLACK)
    var _xAxisPos = t.apply(0, 0)._2
    g.drawLine(0, _xAxisPos.toInt, 400, _xAxisPos.toInt)
    var _yAxixPos = t.apply(0, 0)._1
    g.drawLine(_yAxixPos.toInt, 0, _yAxixPos.toInt, 400)


    // Draw a f(x)=x function
    val o = p.evaluateWithinInterval(-4.0f, 4.0f, 0.01f)

    g.setPaint(Color.BLUE)
    var prev_point = o(0)
    for(point <- o){
      if(prev_point != point){
        val p1 = t.apply(prev_point._1, prev_point._2)
        val p2 = t.apply(point._1, point._2)
        g.drawLine(p1._1.toInt, p1._2.toInt, p2._1.toInt, p2._2.toInt)
      }
      prev_point = point
    }
  }

  this.preferredSize = new Dimension(400, 400);
}
