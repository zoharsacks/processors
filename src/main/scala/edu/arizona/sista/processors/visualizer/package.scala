/**
 * Package level definitions for the processors.visualizer package.
 * Written By: Tom Hicks. 2/6/2015.
 * Last Modified: Initial creation.
 */
package edu.arizona.sista.processors

package object visualizer {

  type TimingsType = scala.collection.mutable.Map[String, java.lang.Long]

  def TimingsType () = scala.collection.mutable.Map[String, java.lang.Long]()

  def TimingsType (entries:List[Tuple2[String, java.lang.Long]]) = {
    scala.collection.mutable.Map[String, java.lang.Long]() ++= entries
  }

  // return a map of start time, stop time, and elapsed time for the given block
  def timeIt (prefix:String, it: => Unit): TimingsType = {
    val start = System.currentTimeMillis()
    it
    val stop = System.currentTimeMillis()
    return scala.collection.mutable.Map((s"${prefix}.start")->start,
                                        (s"${prefix}.stop")->stop,
                                        (s"${prefix}.elapsed")->(stop-start))
  }

}
