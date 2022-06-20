package org.calculator

import java.awt.{Color, Graphics2D}
import scala.swing.{Dimension, Panel}

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
  }

  this.preferredSize = new Dimension(400, 400);
}
