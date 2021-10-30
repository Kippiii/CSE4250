// Author: Ian Orzel, iorzel2019@my.fit.edu
// Course: CSE 4250, Fall 2021
// Project: Proj2, The Color of Flags

import java.awt.Color
import java.awt.image.BufferedImage
import java.util.Scanner
import java.net.URL
import javax.imageio.ImageIO

import kotlin.math.sqrt
import kotlin.math.abs
import kotlin.math.roundToInt

/*
     Data structure for storing the color of an object
*/
class MyColor(var name : String, var red : Int, var green : Int, var blue : Int) {
     var counter = 0

     /*
          Returns the name of the color
     */
     fun retrieveName() : String {
          return this.name
     }
     /*
          Increments the counter of the number of instances of the color
     */
     fun incCounter() {
          this.counter++
     }
     /*
          Returns the current value of the counter
     */
     fun retrieveCounter() : Int {
          return this.counter
     }
     /*
          Returns the Euclidean distance between this color and another color
     */
     fun calcDist(c : Color) : Int {
          return (this.red - c.red)*(this.red - c.red) + (this.green - c.green)*(this.green - c.green) + (this.blue - c.blue)*(this.blue - c.blue)
     }
}

fun main(args : Array<String>) {
     //Read color input from stdin and create color for each
     var colors : ArrayList<MyColor> = ArrayList<MyColor>()
     var sc : Scanner = Scanner(System.`in`)
     while (sc.hasNext()) {
          var name : String = sc.next()
          var red : Int = sc.nextInt()
          var green : Int = sc.nextInt()
          var blue : Int = sc.nextInt()
          colors.add(MyColor(name, red, green, blue))
     }

     //Loop through each flag given
     for (i in 0..args.size-1) {
          //For each flag, get the BufferedImage object
          var url : URL = URL("https://cs.fit.edu/~ryan/images/flags/large/${args[i]}.png")
          var image : BufferedImage = ImageIO.read(url.openConnection().getInputStream())

          //Loop through each pixel of the within the image
          for (x in 0..image.getWidth()-1) {
               for (y in 0..image.getHeight()-1) {
                    val color : Color = Color(image.getRGB(x, y))
                    //Loop through each color to find while color is closest
                    var bestColor : MyColor = colors[0]
                    var minDist : Int = 500*500
                    for (c in colors) {
                         var dist : Int = c.calcDist(color)
                         if (dist < minDist) {
                              bestColor = c
                              minDist = dist
                         }
                    }

                    //Increment the color that is closest
                    bestColor.incCounter()
               }
          }
     }

     //Calculate the total number of pixels
     var totalPixels : Int = 0
     for (c in colors) {
          totalPixels += c.retrieveCounter()
     }

     // Sort the colors for printing
     var sortedColors : List<MyColor> = colors.sortedWith(compareBy({it.retrieveCounter()})).reversed()

     //Print the final data
     for (c in sortedColors) {
          var newVal : Int = (c.retrieveCounter()*1.0 / totalPixels * 1000).roundToInt()
          println("${c.retrieveName()} ${newVal}")
     }
}
