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

      // val formulaInput = new TextField() // utiliser Panel au lieu de Input, car un bouton doit aussi être inséré
      val formulaInput = new BorderPanel {
        add(new TextField, BorderPanel.Position.Center)
        add(new Button("Go"), BorderPanel.Position.East)
      }
      val canvas = new Canvas {
      }

      /*contents = new FlowPanel {
        contents += new Label("Launch rainbows: ")
        contents += new Button("Click me") {
          reactions += {
            case event.ButtonClicked(_) =>
              println("All the colours")
          }
        }
        contents+= canvas
      }*/

      contents = new BorderPanel {
        add(canvas, BorderPanel.Position.Center)
        add(formulaInput, BorderPanel.Position.South)
      }

      pack()
      centerOnScreen()
      open()
    }
    println("Hello world from the main function")
  }

}
