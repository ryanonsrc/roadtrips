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

import scala.collection.mutable

import regions.{Vertex, Graph}

/**
 * RoadTrips/Core :: BidirectionalPolicy
 *
 * This Graph policy requires that for every edge going between two vertices, there must be a
 * second edge going between those same two vertices, in the opposite direction.
 */
trait BidirectionalPolicy[V <: Vertex] extends PolicyModel[V] with EdgeOrientationTracker[V] {

  private val edges = mutable.Map[EdgeOrientation, Boolean]()
  private var unidirectional : Int = 0

  override protected def usePolicy(edge: Graph[V]#GraphEdge) : Unit = {
    // The cases not explicitly checked for both occur when a redundant edge is added (which should be ignored by this policy).
    if (!(edges contains edge) && !(edges contains edge.inverse)) {
      edges.put(edge, false)
      unidirectional += 1
    }
    else if (!(edges contains edge) && (edges contains edge.inverse)) {
      edges.put(edge, true)
      unidirectional -= 1
    }

    super.usePolicy(edge)
  }

  override def infractions : Seq[String] = (unidirectional match {
      case 0 => Seq()
      case vertexCount => Seq("#bidirectional: Bidirectionality is not satisfied, there exists %d pairs of vertices where an edge is going only one direction.".format(vertexCount))
    }) ++ super.infractions
}