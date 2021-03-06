package cs2.particles

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import cs2.util.Vec2

object ParticleSystemApp extends JFXApp{
    stage = new JFXApp.PrimaryStage{
        title = "Particles"
        scene = new Scene(600,600){
            val canvas = new Canvas(600,600)
            content = canvas
            val g = canvas.graphicsContext2D

            val p = new Particle(new Vec2(200,200))
        }
    }
}