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

package io.nary.roadtrips.core
package regions
package models

import places._

import collection.mutable.ListBuffer
import collection.mutable

/**
* RoadTrips/Core :: FoldingGraphModel
*
* Represents a mapping of vertices to "buckets" within a region subdivided in the form of a grid.
* This model is optimized for edges formed in a "linear" fashion (where X only connects to a
* "previous vertex" and a "next vertex", forming a "segment").  In cases such as this the segment grows
* in one direction.  Adding edges that do not follow this rule will simply be stored as a new segment.
* At any point in time invoking the method "fold" will connect deficient vertices within each "bucket",
* hence converting the graph into a connected one. Vertices are considered "deficient" if they also
* exist as keys in the segmentFrontToEdges map.
*/
trait FoldingGraphModel[P <: Point] extends VertexUnification[P] { self: Graph[P] with GraphModel[P] =>

  private val foldedEdges = mutable.Set[Graph[P]#GraphEdge]()
  private val coreEdges = mutable.Set[Graph[P]#GraphEdge]()

  private implicit val expansion = new ExpansionMap[P]

  private val buckets : mutable.LinkedHashMap[Bucket, ListBuffer[P]] = {
    val map = mutable.LinkedHashMap[Bucket, ListBuffer[P]]()
    (0 to lateralResolution).foreach( i => {
      (0 to longitudinalResolution).foreach( j => {
        map += (Bucket(i,j) -> ListBuffer[P]())
      })
    })
    map
  }

  private val lateralScale = Scale(latitudeBounds, lateralResolution)
  private val longitudinalScale = Scale(longitudeBounds, longitudinalResolution)

  /**
   * Get all adjacent and diagonally-neighboring ("kitty-corner") buckets.
   */
  private def nearbyBuckets(bucket : Bucket) : Seq[Bucket] = {
    val result = mutable.Seq[Bucket]()

    Range(-1, 2).foreach { di =>
      Range(-1, 2).foreach { dj =>
        (bucket.lateralIndex + di, bucket.longitudinalIndex + dj) match {
          case (i, j) if i >=0 && i <= lateralResolution && j >=0 && j <= longitudinalResolution => result :+ Bucket(i,j)
          case _ =>
        }
      }
    }

    result
  }

  protected def mapToBucket(point: P) = {
    val bucket = point.zeroBasedBucket(lateralScale, longitudinalScale)

    bucket
  }

  protected def verticesOfBucketForThisPoint(point : P) : Option[ListBuffer[P]] = {
    mapToBucket(point) match {
      case None => None
      case Some(bucket) => Some(buckets(bucket))
    }
  }

  // This impl of addNeighbor will extend a pre-existing segment or create a new one
  protected def addNeighbor(edge: Graph[P]#GraphEdge) : Unit = {
    if (!foldedEdges.contains(edge))
      coreEdges += edge

    val (originBucket, destBucket) = (mapToBucket(edge.origin), mapToBucket(edge.destination))

    if (originBucket != None) {
      buckets(originBucket.get) += edge.origin
    }

    if (destBucket != None) {
      buckets(destBucket.get) += edge.destination
    }

    unify(edge.origin)
    unify(edge.destination)
  }

  // note whatever impl we do of this will *not* be very efficient
  protected def getNeighbors(vertex: P): Seq[Graph[P]#GraphEdge] = throw new NotImplementedError("getNeighbors() not implemented for this graph model.")

  protected def getBucketInfo(point: P) : String = mapToBucket(point).map(b => s"""${b.lateralIndex.toString}, ${b.longitudinalIndex.toString}""").getOrElse("NONE")

  protected def annotate(point: P)(f: P => String) = new Point(point.latitude, point.longitude) {
    override def label = f(point)
    override def toString = label
  }

  def foldWith(vertices: Set[P]) : Unit                               // Folds with a set of vertices
  def foldWith(vertices: Set[P], nearbyRegions: Set[Set[P]]) : Unit   // Folds across neighboring regions

  protected def foldCross(vertices: Set[P]) : Unit = {
    val newlyConnected = mutable.Set[P]()

    vertices.foreach {
      case v if !newlyConnected.contains(v) =>
        // get the nearest vertices that isn't the same
        val nearby = expand(v).toList.sortBy(v.squaredDistanceFrom(_)).filter(_ != v)

        if (nearby.nonEmpty) {
          foldUsing(v, nearby.head)
          newlyConnected += nearby.head
        }
      case _ =>
    }
  }

  protected def foldUsing(start: P, end: P) : Unit = {
    val edge = Edge[P, P](start, end, costOfFoldedEdge(start, end))
    foldedEdges += edge
    this += edge // since Graph.+= invokes addNeighbor(): segment update/creation will be taken care of.
  }

  // complete the graph across all buckets.
  def fold : Unit = {
    buckets.values.foreach(vertices => foldWith(vertices.toSet))
    buckets.foreach{ case (bucket, vertices) =>
      foldWith(vertices.toSet, nearbyBuckets(bucket).map(buckets(_).toSet).toSet)
    }
  }

  /////////// Acquire Bucket Information: Generally for debugging/data-analysis purposes

  private def occupiedBucketInfo[I](bucketInfo: ((Bucket, ListBuffer[P])) => I) : Map[Bucket, I] = buckets.filter{ case (_, v) => !v.isEmpty }.map { case (k, v) => {
    (k, bucketInfo((k, v)))
  }}.toMap

  def occupiedBucketVertices : Map[Bucket, List[P]] = occupiedBucketInfo[List[P]](_._2.toList)
  def occupiedBucketBoundaries : Map[Bucket, BucketBounds] = occupiedBucketInfo[BucketBounds]{ case(k, _) => k.bounds(lateralScale, longitudinalScale) }

  def allSegmentEdges : Set[EdgeWrapper] = coreEdges.toSet.map((edge : Graph[P]#GraphEdge) => EdgeWrapper(edge.origin, edge.destination))
  def allFoldedEdges : Set[EdgeWrapper] = foldedEdges.toSet.map((edge : Graph[P]#GraphEdge) => EdgeWrapper(edge.origin, edge.destination))

  protected def costOfFoldedEdge(v: Point, w: Point) : Double

  protected val latitudeBounds : LatitudeBounds
  protected val longitudeBounds : LongitudeBounds
  protected val lateralResolution : Int             // number of lateral buckets
  protected val longitudinalResolution : Int        // number of longitudinal buckets
}

case class Bound(latitude: Int, longitude: Int)
case class ProximityBucket(latitudeIndex: Int, longitudeIndex: Int)

case class EdgeWrapper(origin: Point, destination: Point) {
  def toJson = s"""{"origin": ${origin.toJson}, "destination": ${destination.toJson}}"""
}
