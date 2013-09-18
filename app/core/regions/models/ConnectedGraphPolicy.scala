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

import collection.mutable
import scala.Some

/**
 * RoadTrips/Core :: ConnectedGraphPolicy
 *
 * The CompleteGraphPolicy requires that for a given vertex within the graph, there exists
 * a path leading to all other vertices in the graph.
 */
trait ConnectedGraphPolicy[V <: Vertex, T] extends PolicyModel[V] with DualPartitionable[T, V] {

  private def islands = vertexIslands.values.toSet
  private implicit val islandExpansion = new ExpansionMap[V]()

  private def islandsExpanded = (Set[Set[V]]() /: islands) { case (setOfSets, island) =>
    setOfSets + (Set[V]() /: island) { case (set, vertex) =>
      set + vertex
    }
  }

  private val vertexIslands = mutable.Map[V, Set[V]]()
  private val tagPartitions = mutable.Map[T, mutable.Set[V]]()

  private case class Membership(vertex: V, set: Set[V])

  override def usePolicy(edge: Graph[V]#GraphEdge) : Unit = {
    val origin = edge.origin
    val destination = edge.destination

    val originEnclosure : Option[Set[V]] = vertexIslands.get(origin)
    val destinationEnclosure : Option[Set[V]] = vertexIslands.get(destination)

    val island : Set[V] = (originEnclosure, destinationEnclosure) match {
      case (None, None) =>                  Set[V](origin, destination)
      case (Some(originSet), None) =>       originSet + destination
      case (None, Some(destinationSet)) =>  destinationSet + origin
      case (Some(originSet), Some(destinationSet)) => originSet ++ destinationSet
    }

    island.foreach(vertexIslands += _ -> island)

    super.usePolicy(edge)
  }

  def groupByPrimary : Map[T, Set[V]] = tagPartitions.mapValues(_.toSet).toMap
  def groupBySecondary : Set[Set[V]] = islandsExpanded.zip(Range(0, islands.size)).groupBy { case (_, index) => index }.map{ case (_, value) => value.head._1.toSet }.toSet

  def islandIndeces(vertex: V) : Set[Int] = {
    val indexedIslands = islandsExpanded.zip(Range(0, islands.size)).groupBy { case (_, index) => index }.mapValues(values => values.head)

    indexedIslands.flatMap { case (index, (vertices, _)) => (vertices.filter(_ == vertex) match {
      case filteredVertices if filteredVertices.size != 0 => Set[Int](index)
      case _ => Set[Int]()
    })}.toSet
  }

  def partition(tag: T, vertex: V) : Unit = {
    tagPartitions.getOrElseUpdate(tag, mutable.Set[V]()) += vertex
  }

  override def infractions = (islands.size match {
      case 1 => Seq()
      case islandCount => Seq("#connected: Graph is not fully connected, graph is split into %d islands.".format(islandCount))
    }) ++ super.infractions
}
