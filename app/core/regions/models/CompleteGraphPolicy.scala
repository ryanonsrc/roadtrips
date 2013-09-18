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

import regions.{Vertex, Graph}
import scala.collection.mutable

/**
 * RoadTrips/Core :: CompleteGraphPolicy
 *
 * The CompleteGraphPolicy requires that for a given vertex in the graph, there exists an edge
 * to each and every other vertex (excluding the origin vertex itself).
 */
trait CompleteGraphPolicy[V <: Vertex] extends PolicyModel[V] with EdgeOrientationTracker[V] {

  private val vertexDegrees = mutable.Map[V, Long]()
  private val edges = mutable.Set[EdgeOrientation]()

  private def incrementVertexDegree(vertex: V) : Unit = {
    vertexDegrees += vertex -> (vertexDegrees.getOrElse(vertex, 0L) + 1)
  }

  override protected def usePolicy(edge: Graph[V]#GraphEdge) : Unit = {
    if (!edges.contains(edge) && !edges.contains(edge.inverse)) {
      edges += edge
      incrementVertexDegree(edge.origin)
      incrementVertexDegree(edge.destination)
    }

    super.usePolicy(edge)
  }

  override def infractions : Seq[String] = {
    var infractionSeq = mutable.ListBuffer[String]()

    vertexDegrees.toStream.takeWhile(_ => infractionSeq.size == 0).foreach(mapEntry => {
      val (vertex, degree) = mapEntry
      if (degree < vertexDegrees.size - 1)
        infractionSeq += "#complete: Graph is incomplete. Vertex (%s) is of degree >= %d, actual = %d".format(vertex.toString, vertexDegrees.size - 1, degree)
    })

    infractionSeq ++ super.infractions
  }
}

