package org.calculator

import java.awt.{BasicStroke, Color, Graphics2D, Stroke}
import scala.swing.{Dimension, Panel}
import scala.util.control.Breaks.break

class Canvas extends Panel {
  override def paintComponent(g: Graphics2D): Unit ={
    val t = new Transform(200, 200, 80) // Should be an element property

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
    g.setColor(Color.BLACK)
    var _xAxisPos = t.apply(0, 0)._2
    g.drawLine(0, _xAxisPos.toInt, 400, _xAxisPos.toInt)
    var _yAxixPos = t.apply(0, 0)._1
    g.drawLine(_yAxixPos.toInt, 0, _yAxixPos.toInt, 400)
  }

  this.preferredSize = new Dimension(400, 400);
}
