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

import models.{PolicyModel, GraphModel}
import scala.collection.mutable

/**
 * RoadTrips/Core :: Graph
 *
 * A graph contains a network of places, which are connected through the utilization
 * of one or more Place Mapping strategy.  An edge from place U to place V is inferred by
 * a models from U to V.  Such implies that there is also an inferred edge from V to U.  The only
 * distinction between the edges is that they may have different costs.
 *
 * Each edge within a graph does not necessarily correspond to a single road, nor does a place
 * necessarily correspond to a single landmark or point of interest.  Only sub-classes of this
 * Graph will truly define how find-grained these edges and vertices truly are.
 */
abstract class Graph[V <: Vertex] extends GraphModel[V] with PolicyModel[V] {
  type GraphEdge = Edge[_ <: V,_ <: V]        // origin and dest can be different sub-classes of type P
  protected final def getOrigin(edge: Graph[V]#GraphEdge) = edge.origin

  private val places = mutable.Set[V]()

  final def += (edge: GraphEdge) = {
    places += (edge.origin)
    places +=(edge.destination)
    addNeighbor(edge)
    usePolicy(edge)
    this
  }
}

case class Edge[V <: Vertex, W <: Vertex](origin: V, destination: W, cost: Double) {
  def inverse = Edge[W, V](destination, origin, cost)
  def parallel(other: Edge[Vertex, Vertex]) : Boolean = origin == other.origin && destination == other.destination
}

trait Vertex {
}