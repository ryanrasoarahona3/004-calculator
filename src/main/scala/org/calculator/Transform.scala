package org.calculator

/**
 * La classe Transform permet de transformer un système de coordonnées en pixels affichable à l'écran
 */
class Transform (xc: Int, yc: Int, z: Int){
  private var xCenter: Int = 0     // Coordonnées du centre (en pixel) - nombre entier
  private var yCenter: Int = 0     // Coordonnées du centre (en pixel) - nombre entier
  private var zoom: Float = 0.0f      // Une unité réel correspond à combien d'unité en pixel

  xCenter = xc
  yCenter = yc
  zoom = z

  def apply(x: Float, y: Float): (Float, Float) ={
    val outputX = xCenter + x*zoom
    val outputY = yCenter + y*zoom
    (outputX, outputY)
  }
}
