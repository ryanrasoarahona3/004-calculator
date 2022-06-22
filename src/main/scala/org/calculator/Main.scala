package org.calculator

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{Color, Graphics2D}
import javax.swing.BorderFactory
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    new Frame {
      title = "Hello world"

      val goButton = new Button("Go")
      val formulaField = new TextField()
      // val formulaInput = new TextField() // utiliser Panel au lieu de Input, car un bouton doit aussi être inséré
      val formulaInput = new BorderPanel {
        add(new Label("f(x)="), BorderPanel.Position.West)
        add(formulaField, BorderPanel.Position.Center)
        add(goButton, BorderPanel.Position.East)
      }
      formulaInput.border = BorderFactory.createEmptyBorder(5,5,5,5)

      val canvas = new Canvas {
      }

      listenTo(goButton)
      reactions+= {
        case ButtonClicked(`goButton`) => {
          val p = new TreeNode2(formulaField.text)
          canvas.p = p
          canvas.repaint()
        }
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
