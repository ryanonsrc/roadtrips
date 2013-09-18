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

import core.regions.places.Point
import collection.mutable
import core.regions.DualPartitionable

/**
 * RoadTrips/Data :: HighwayUnification
 *
 * Unifies segments of highways that are apart of the same highway (even for highways with multiple names).
 */
trait HighwayUnification { self : DualPartitionable[String, Point] =>
  protected def buildHighwayTransitiveRelation(allHighwaysByPoint : Map[Point, String]) : Map[Int, Set[String]] = {
    val map = mutable.Map[Int, mutable.Set[String]]()

      allHighwaysByPoint.values.foreach { name => {
        val numbers = ((Set[Int]() /: highwayNumbers(name)) (_ + _))      // All highway numbers
        val unMerged = mutable.Set[Int]()                                 // Highway numbers with unMerged Aliases

        var aliases = mutable.Set[String]()

        // Attempt to find a pre-existing set for this alias ...
        numbers.foreach { number =>
          map.get(number) match { case Some(foundAliases) => aliases = foundAliases case None => Unit }
        }; aliases += name  // add the alias

        // for each highway number ...
        numbers.foreach { number =>
          map.get(number) match {
            // .. If we have no aliases for that number we add it to the "unMerged" set
            case None =>
              unMerged += number
            // .. If we already have some aliases we merge and point aliases ref to it
            case Some(foundAliases) => {
              foundAliases ++= aliases
              aliases = foundAliases
            }
          }
        }

        // For all previously unmerged indices: we must point to the complete aliases set.
        unMerged.foreach(map += _ -> aliases)
      }
    }

    map.toMap.mapValues(_.toSet)
  }

  protected def highwayNumbers(highwayName: String) : Set[Int] = "[^0-9]+".r.split(highwayName).flatMap(possibleNumber => {
    val pattern = "([0-9]+)".r
    possibleNumber match {
      case pattern(_) => Set[Int](possibleNumber.toInt)
      case _ => Set[Int]()
    }
  }).toSet


  // Generates a set of all highway name aliases (including the supplied name) derived via transitivity
  private def allHighwayNameAliases(highway: String, transitiveRelation: Map[Int, Set[String]]) : Set[String] = highwayNumbers(highway).flatMap(transitiveRelation(_))

  // Generates a set of all highway points from name aliases derived via groupByPrimary (highway names -> points)
  private def highwayPointsFromAliases(highwayAliases: Set[String]) : Set[Point] = highwayAliases.flatMap(groupByPrimary(_))

  // Generates a set of sets of points (where each inner set represents the points for a given island).
  protected def islands(points: Set[Point]) : Set[Set[Point]] = (Set[Set[Point]]() /: groupBySecondary) ( (allIslands, island) =>
    allIslands + island.filter(points contains _)
  ).filter(!_.isEmpty)

  protected def foldOverAllIslands(allEndPoints: Set[Point], fold: Set[LinearIslandSpan] => Unit) : Unit = {
    fold(islands(allEndPoints).map(deriveSpan(_)))
  }

  protected def foldHighways(allEndPoints: Set[Point], transitiveRelation : Map[Int, Set[String]], fold: Set[LinearIslandSpan] => Unit, allHighwaysByPoint : Map[Point, String]) : Unit = {
    allEndPoints.map(allHighwaysByPoint(_)).foreach(highway => {
        val points = highwayPointsFromAliases(allHighwayNameAliases(highway, transitiveRelation)) & allEndPoints

        fold(islands(points).flatMap(deriveMultiSpan(_)))
      }
    )
  }

  private def deriveMultiSpan(island: Set[Point]) : Set[LinearIslandSpan] = island.size match {
    case 2 => Set(LinearIslandSpan(island.head, Some(island.last)))
    case 1 => Set(LinearIslandSpan(island.head))
    case 0 => throw new Exception("Island must contain at least one point!")
    case n => Set(LinearIslandSpan(island.head, Some(island.last))) ++ deriveMultiSpan(island - island.head - island.last)
  }

  private def deriveSpan(island: Set[Point]) : LinearIslandSpan = island.size match {
    case 2 => LinearIslandSpan(island.head, Some(island.last))
    case 1 => LinearIslandSpan(island.head)
    case 0 => throw new Exception("Island must contain at least one point!")
    // TODO: We may need to be a bit more crafty in how we choose two endpoints from an island.
    case n => {println(s"We have ${n} 'endpoints' in this island"); LinearIslandSpan(island.head, Some(island.last))}
  }
}

object PointChoice extends Enumeration {
  type PointChoice = Value
  val AnchorChoice, AuxChoice = Value
}
import PointChoice._

abstract class AnchorAndOptionalAux(anchor: Point, aux: Option[Point] = None) {
  def length = aux match {
    case Some(point) => anchor.squaredDistanceFrom(point)
    case None => Double.PositiveInfinity
  }
}

case class LinearIslandSpan(anchor: Point, aux: Option[Point] = None) extends AnchorAndOptionalAux(anchor, aux) {
  def createPotential(fromPoint: PointChoice, toSpan: LinearIslandSpan, toPoint: PointChoice) : Option[PotentialFoldedEdge] = {
    def point(span: LinearIslandSpan, choice: PointChoice) = choice match {
      case AnchorChoice => (Some(span.anchor), span.aux)
      case AuxChoice => (span.aux, Some(span.anchor))
    }

    val from = point(this, fromPoint)
    val to = point(toSpan, toPoint)

    (from, to) match {
      case ((Some(fromHere), fromOther),(Some(toHere), toOther)) => Some(PotentialFoldedEdge(fromHere, this, LinearIslandSpan(toHere, toOther)))
      case _ => None
    }
  }
}

case class PotentialFoldedEdge(endOfPrevious: Point, previousIslandSpan: LinearIslandSpan, nextIslandSpan: LinearIslandSpan) extends AnchorAndOptionalAux(endOfPrevious, nextIslandSpan.aux)
