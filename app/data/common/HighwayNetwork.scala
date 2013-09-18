/**
 * Copyright (c) 2013, Ryan Delucchi (nary.io)
 * All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *   1.) Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *   2.) Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *   3.) Neither the name of the nary.io nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL RYAN DELUCCHI BE LIABLE FOR ANY
 *  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package io.nary.roadtrips
package data
package common

import core.regions.{Graph, Edge}
import core.regions.places._
import core.regions.models.{ConnectedGraphPolicy, FoldingGraphModel}
import scala.collection.{mutable}
import core.regions.places.Point
import core.regions.places.LongitudeBounds
import core.regions.places.LatitudeBounds

/**
 * RoadTrips/Data :: HighwayNetwork
 *
 * Represents a bounded highway network (as a graph) with a chosen resolution, including the ability to unify points, highways
 * storing the graph via the FoldingGraphModel using the ConnectedGraphPolicy.
 */
class HighwayNetwork(override val latitudeBounds: LatitudeBounds, override val longitudeBounds: LongitudeBounds, override val lateralResolution: Int, override val longitudinalResolution: Int)
  extends Graph[Point] with PointUnification with HighwayUnification with FoldingGraphModel[Point] with ConnectedGraphPolicy[Point, String] {
  protected def costOfFoldedEdge(v: Point, w: Point): Double = v.distanceFrom(w)

  private val allHighwaysByPoint = mutable.Map[Point, String]()

  // All other collections use rounded-points.  This one is the exception (since we need to keep track of each highway
  // direction separately to determine deadendedness).
  private val deadEndedness = mutable.Map[Point, Boolean]()

  private val highwayPoints = mutable.LinkedHashSet[Point]()
  private val segmentPoints = mutable.LinkedHashSet[Point]()

  // Once evaluated, any new highways added will not be included (which could result with in-complete folding).
  private lazy val highwayTransitiveRelation : Map[Int, Set[String]] = buildHighwayTransitiveRelation(allHighwaysByPoint.toMap)
  lazy val deadEnds : Set[Point] = deadEndedness.filter { case(point, cond) => cond == true }.map { case(point, _) => point }.toSet

  //////// Dead-Ends annoted with information on bucket and/or island residency

  def bucketAnnotatedDeadEnds = deadEnds.map(annotate(_)(getBucketInfo))

  def islandAnnotatedDeadEnds = deadEnds.map(annotate(_) { point =>
    islandIndeces(point).size > 0 match {
      case true => islandIndeces(point).mkString(",")
      case false => "NIM"  // "No island membership"
    }
  })

  def annotatedDeadEnds = deadEnds.map(annotate(_) { point =>
    val bucketVertices = verticesOfBucketForThisPoint(point)

    bucketVertices match {
      case None => "No Bucket"
      case Some(vertices) => {
        val points = deadEnds.filter(vertices.contains(_))
        val pointIslands = islands(points)

        points.size.toString + ": " + pointIslands.map(_.size).mkString("(", ",", ")")
      }
    }
  })

  // Ends a segment that is being constructed
  def endSegment : Unit = {
    segmentPoints.foreach( point => highwayPoints.add(point) )
    segmentPoints.clear
  }

  // Add a point to the segment
  def +=(point: Point) : HighwayNetwork = {
    if (segmentPoints.size != 0) {
      val previousPoint = segmentPoints.head
      this += Edge[Point, Point](previousPoint, point, previousPoint distanceFrom point)
    }

    segmentPoints += point

    this
  }

  // Create an edge in the graph
  override def addNeighbor(edge: Graph[Point]#GraphEdge) : Unit = {
    val (originFlag, destinationFlag) = (deadEndedness.get(edge.origin), deadEndedness.get(edge.destination)) match {
      case (None, None) => (true, true)
      case (Some(_), None) => (false, true)
      case (None, Some(_)) => (true, false)
      case (Some(_), Some(_)) => (false, false)
    }

    deadEndedness += edge.origin -> originFlag
    deadEndedness += edge.destination -> destinationFlag

    super.addNeighbor(edge)
  }

  def endHighway(name: String) : Unit = {
    highwayPoints.foreach( point => {
      allHighwaysByPoint += (point -> name)
      partition(name, point)
    })

    highwayPoints.clear
  }

  private def fold(spans: Set[LinearIslandSpan]) : Unit = {
    val matrix : Set[(LinearIslandSpan, LinearIslandSpan)] = (for ( x <- spans; y <- spans; if x ne y )
      yield Set[LinearIslandSpan](x, y)).map(span => (span.head, span.last))

    val edgeSpace = (Seq[Seq[(Point, Point)]]() /: matrix) { case (allSeq, (x, y)) =>
        val edges = Seq[(Option[Point], Option[Point])]((Some(x.anchor), Some(y.anchor)), (Some(x.anchor), y.aux),
          (x.aux, Some(y.anchor)), (x.aux, y.aux))

        val validEdges : Seq[(Point, Point)] = edges.flatMap {
          case (Some(a), Some(b)) => Seq[(Point, Point)]((a, b))
          case _ => Seq[(Point, Point)]()
        }

        allSeq :+ validEdges
    }

    edgeSpace.foreach { pairs =>
      val shortest = pairs.sortBy { case (a, b) => a.squaredDistanceFrom(b) }.head
      foldUsing(shortest._1, shortest._2)
    }
  }

  def foldWith(vertices: Set[Point]) : Unit = {
    val localDeadEnds = deadEnds.filter(vertices.contains(_))
    foldHighways(localDeadEnds, highwayTransitiveRelation, fold, allHighwaysByPoint.toMap)
    foldCross(localDeadEnds)
  }

  def foldWith(vertices: Set[Point], nearbyRegions: Set[Set[Point]]) : Unit = {
    nearbyRegions.foreach { case regionVertices =>
      val localDeadEnds = deadEnds.filter((vertices ++ regionVertices).contains(_))
      foldHighways(localDeadEnds, highwayTransitiveRelation, fold, allHighwaysByPoint.toMap)
    }
  }
}