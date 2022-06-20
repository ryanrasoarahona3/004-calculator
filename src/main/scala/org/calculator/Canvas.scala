package org.calculator

import java.awt.{Color, Graphics2D}
import scala.swing.{Dimension, Panel}
import scala.util.control.Breaks.break

class Canvas extends Panel {
  override def paintComponent(g: Graphics2D): Unit ={
    g.clearRect(0, 0, size.width, size.height)

    g.setColor(Color.blue)
    g.fillOval(0, 0, 100, 100)


    // Tracer les grilles
    // Doit trouver un système de test pour automatiser la création des grilles
    // Une matrice de transforamtion devrait aussi être utilisé pour la conversion des coordonnées
    g.setColor(Color.gray)
    g.drawLine(200, 0, 200, 400)
    val t = new Transform(200, 200, 80)
    // Vertical Right Dark Gray
    for(x <- 0 to 4){
      var _x = t.apply(x, 0)._1
      g.drawLine(_x.toInt, 0, _x.toInt, 400)
      print("Hi")
    }
  }

  this.preferredSize = new Dimension(400, 400);
}
