package org.calculator

import scala.swing._

object Main {
  def main(args: Array[String]): Unit = {

    new Frame {
      title = "Hello world"

      contents = new FlowPanel {
        contents += new Label("Launch rainbows: ")
        contents += new Button("Click me") {
          reactions += {
            case event.ButtonClicked(_) =>
              println("All the colours")
          }
        }

        
      }

      pack()
      centerOnScreen()
      open()
    }
    println("Hello world from the main function")
  }

}
