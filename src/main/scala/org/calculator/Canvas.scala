package org.calculator

import java.awt.{Color, Graphics2D}
import scala.swing.Panel

class Canvas extends Panel {
  override def paintComponent(g: Graphics2D): Unit ={
    g.clearRect(0, 0, size.width, size.height)

    g.setColor(Color.blue)
    g.fillOval(0, 0, 100, 100)
  }
}
