package org.calculator

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{Color, Graphics2D}
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    new Frame {
      title = "Hello world"

      val formulaInput = new TextField()

      val canvas = new Canvas {
        preferredSize = new Dimension(100, 100)
      }

      contents = new FlowPanel {
        contents += new Label("Launch rainbows: ")
        contents += new Button("Click me") {
          reactions += {
            case event.ButtonClicked(_) =>
              println("All the colours")
          }
        }
        contents+= canvas
      }

      pack()
      centerOnScreen()
      open()
    }
    println("Hello world from the main function")
  }

}
