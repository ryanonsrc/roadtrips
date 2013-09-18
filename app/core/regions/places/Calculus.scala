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
package core
package regions
package places

import tools.Math._
import scala.math._

/**
 * RoadTrips/Core :: Calculus
 *
 * Spatial calculations and data structures.
 */
trait Calculus { self: Point =>

  // Is only accurate for short distances (as it is computed on a cartesian plane ... use with care.
  def cartesianDistanceFrom(there: Point) = sqrt(pow(latitude - there.latitude, 2) + pow(longitude - there.longitude, 2))

  // Is only intended for computing a distance used for sorting (over short distance)
  def squaredDistanceFrom(there: Point) = pow(latitude - there.latitude, 2) + pow(longitude - there.longitude, 2)

  // Compute the distance from another point using The Haversine Formula
  // see: http://www.movable-type.co.uk/scripts/latlong.html
  //
  def distanceFrom(there: Point) = {
    val radiusOfTheEarthInKm = 6371
    val diffLat = toRadians(there.latitude-latitude)
    val diffLong = toRadians(there.longitude-longitude)
    val hereLat = toRadians(latitude)
    val thereLat = toRadians(there.latitude)

    val haversine = sin(diffLat/2) * sin(diffLat/2) +
      sin(diffLong/2) * sin(diffLong/2) * cos(hereLat) * cos(thereLat)

    radiusOfTheEarthInKm * 2 * atan2(sqrt(haversine), sqrt(1-haversine))
  }


  // Compute the inverse squared distance (note: a value > maximum will never be returned, even if the result is infinity)
  // A larger coefficient yields a value that drops more suddenly as this is further from center.
  def inverseSquaredDistanceFrom(center: Point, coefficient: Double = 1, maximum: Double = Double.PositiveInfinity) = {
    val squared = pow(coefficient * distanceFrom(center), 2)

    min(maximum, squared match {
      case 0 => Double.PositiveInfinity
      case _ => 1/squared
    })
  }

  // Is the other point "local" (within a specified radius about this point)?
  def isLocal(there: Place, maximumRadius: Double) = distanceFrom(there) <= maximumRadius

  // Return a bucket that a point given the provided scales, should reside within.
  def zeroBasedBucket(lateral: Scale[LatitudeBounds], longitudinal: Scale[LongitudeBounds]) : Option[Bucket] = {
    if(!bounded(Bound(lateral.bounds.min) INCLUSIVE, Bound(lateral.bounds.max) INCLUSIVE, self.latitude) ||
       !bounded(Bound(longitudinal.bounds.min) INCLUSIVE, Bound(longitudinal.bounds.max) INCLUSIVE, self.longitude)) {

      print(s"Point is out of range: Point(${self.latitude}, ${self.longitude}})")
      None
    }
    else {
      val relativePoint = new UnnamedPoint(self.latitude - lateral.bounds.min, self.longitude - longitudinal.bounds.min)
      val relativeMax = new UnnamedPoint(lateral.bounds.max - lateral.bounds.min, longitudinal.bounds.max - longitudinal.bounds.min)

      val bucket = Bucket(floor((relativePoint.latitude * lateral.numberBuckets) / relativeMax.latitude).toInt,
        floor((relativePoint.longitude * longitudinal.numberBuckets) / relativeMax.longitude).toInt)

      bucket match {
        case Bucket(latIndex, longIndex)
          if latIndex >= 0 && latIndex < lateral.numberBuckets &&
             longIndex >= 0 && longIndex < longitudinal.numberBuckets => Some(bucket)
        case _ => {
          print(s"Bucket out of range: Bucket(${bucket.lateralIndex}, ${bucket.longitudinalIndex})")
          None
        }
      }
    }
  }
}

trait Bounds {
  val min: Double
  val max: Double
}

case class LatitudeBounds(override val min: Double, override val max: Double) extends Bounds
case class LongitudeBounds(override val min: Double, override val max: Double) extends Bounds
case class Scale[B <: Bounds](bounds: B, numberBuckets: Int)

case class Bucket(lateralIndex: Int, longitudinalIndex: Int) {
  def bounds(lateral: Scale[LatitudeBounds], longitudinal: Scale[LongitudeBounds]) : BucketBounds = {

    def dimAt(index: Int, min: Double, max: Double, numberOfBuckets: Int) = ((index * (max - min)) / numberOfBuckets) + min

    def corner(latIndex: Int, longIndex : Int) = new UnnamedPoint(
      dimAt(latIndex, lateral.bounds.min, lateral.bounds.max, lateral.numberBuckets),
      dimAt(longIndex, longitudinal.bounds.min, longitudinal.bounds.max, longitudinal.numberBuckets)
    )

    BucketBounds(
      corner(lateralIndex + 1, longitudinalIndex), corner(lateralIndex + 1, longitudinalIndex + 1),
      corner(lateralIndex, longitudinalIndex + 1), corner(lateralIndex, longitudinalIndex)
    )
  }
}

case class BucketBounds(northWest: Point, northEast: Point, southEast: Point, southWest: Point) {
  def toJson : String = s"""{"northWest":${northWest.toJson}, "northEast":${northEast.toJson}, "southEast":${southEast.toJson}, "southWest":${southWest.toJson}}"""
}