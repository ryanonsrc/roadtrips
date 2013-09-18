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

import scala.collection.mutable
import org.scalatest.FunSuite
import core.regions.places.{Point, UnnamedPoint}
import core.regions.DualPartitionable

/**
 * RoadTrips/Data :: HighwayNetworkTest
 *
 * Test HighwayNetwork functionality
 */
class HighwayNetworkTest extends FunSuite with HighwayUnification with DualPartitionable[String, Point] {

  private val points = (new UnnamedPoint(0, 0), new UnnamedPoint(0, 1), new UnnamedPoint(0, 2), new UnnamedPoint(1, 0), new UnnamedPoint(2, 0), new UnnamedPoint(2, 1))
  private val highway900and905 = "US 900; US 905"
  private val highway905and800 = "US 905; US 800"
  private val highway800and805 = "US 800; US 805"
  private val highway600 = "US 600"
  private val highway700 = "US 700"


  def partition(tag: String, vertex: Point) : Unit = ()

  def groupByPrimary: Map[String, Set[Point]] = Map[String, Set[Point]]()
  def groupBySecondary: Set[Set[Point]] = Set[Set[Point]]()

  private val allHighwaysByPoint : Map[Point, String] = Map(points._1 -> highway800and805, points._2 -> highway905and800,
    points._3 -> highway900and905, points._4 -> highway600, points._5 -> highway700)

  // [point1-- 800:805 --point2-- 800:905 --point3-- 900:905 --point4-- 600 --point5-- 700 --point6]
  private val terminals : mutable.Map[String, Set[LinearIslandSpan]] = mutable.Map(highway800and805 -> Set(LinearIslandSpan(points._1, Some(points._2))),
    highway905and800 -> Set(LinearIslandSpan(points._2, Some(points._3))), highway900and905 -> Set(LinearIslandSpan(points._3, Some(points._4))),
    highway600 -> Set(LinearIslandSpan(points._4, Some(points._5))), highway700 -> Set(LinearIslandSpan(points._5, Some(points._6))))

  test("ensure that buildHighwayTransitiveRelation() is working correctly.") {
    val transitiveRelation = buildHighwayTransitiveRelation(allHighwaysByPoint)
    assert(transitiveRelation.size == 6)

    def assertRelation(nums: Int*)(strings: String*) = {
      nums.foreach(num => assert(transitiveRelation(num).size == strings.size))
      nums.foreach(num => strings.foreach(str => assert(transitiveRelation(num).contains(str))))
    }

    assertRelation(800, 805, 900, 905)("US 905; US 800", "US 800; US 805", "US 900; US 905")
    assertRelation(700)("US 700")
    assertRelation(600)("US 600")
  }
}
