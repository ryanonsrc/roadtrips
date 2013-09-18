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
package places

import collection.mutable

/**
 * RoadTrips/Core :: Place
 *
 * General data structures and operations on the abstract notion of a place.
 */
object Place {
  private val all = mutable.Map[Long, Place]()

  private def intern(id: Long, place: Place) = if (all.contains(id))
      throw new IllegalArgumentException("Place with the specified id already exists");
    else
      all.put(id, place)

  def apply(id: Long) = byId(id)
  def byId(id: Long) = all(id)
}

abstract case class Point(val latitude: Double, val longitude: Double) extends Vertex with Calculus {
  def toJson : String = s"""{"latitude":${latitude}, "longitude":${longitude}, "label":"${label}"}"""
  def label : String = "Undefined Label"
}

abstract class Place(val id: Long, val name: String, override val latitude: Double, override val longitude: Double) extends Point(latitude, longitude) with Vertex {
  Place.intern(id, this)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Place]

  override def equals(other: Any): Boolean = other match {
    case that: Place =>
      (that canEqual this) &&
      id == that.id &&
      name == that.name &&
      longitude == that.longitude &&
      latitude == that.latitude
    case _ => false
  }

  override def hashCode: Int = 31 * (31 * (31 + id.hashCode) + longitude.hashCode) + latitude.hashCode
}

class UnnamedPoint(override val latitude: Double, override val longitude: Double) extends Point(latitude, longitude)
