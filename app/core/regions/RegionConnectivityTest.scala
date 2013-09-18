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

import org.scalatest.FunSuite
import places.{PointOfInterest, Center}
import collection.mutable

/**
 * RoadTrips/Core :: RegionConnectivityTest
 *
 * Used for some testing of core graph classes
 */
class RegionConnectivityTest extends FunSuite {

  case class SampleGraph[G <: Metro, V <: Vertex](graph: G, poi: Seq[PointOfInterest], edges: Seq[Edge[V,V]], cityCenter: Center)

  var nextId = 0; def useId = {
    val prevId = nextId
    nextId+=1
    prevId
  }

  def buildSimpleMetroGraph(skipEdge2c: Boolean = false, skipEdgec2: Boolean = false) = {
    val cityCenter = new Center(useId, "City Center", 0, 0)
    val metro = new Metro(cityCenter)

    val poiOne = new PointOfInterest(useId, "Some crappy amusement park", 1, 1)
    val poiTwo = new PointOfInterest(useId, "Some overpriced museum", 1, 7)

    val edges = mutable.Map(
      'edge12 -> new MetroEdge(poiOne, poiTwo, poiOne distanceFrom poiTwo),
      'edge21 -> new MetroEdge(poiTwo, poiOne, 2 * (poiOne distanceFrom poiTwo)),
      'edge1c -> new MetroEdge(poiOne, cityCenter, poiOne distanceFrom cityCenter),
      'edgec1 -> new MetroEdge(cityCenter, poiOne, 2 * (poiOne distanceFrom cityCenter)),
      'edge2c -> new MetroEdge(poiTwo, cityCenter, poiTwo distanceFrom cityCenter),
      'edgec2 -> new MetroEdge(cityCenter, poiTwo, 2 * (poiTwo distanceFrom cityCenter))
    )

    var neighborsOfTwoSeq = Seq('edge21, 'edge2c)
    var neighborsOfCenterSeq = Seq('edgec1, 'edgec2)

    if (skipEdge2c) {
      edges.remove('edge2c)
      neighborsOfTwoSeq = Seq('edge21)
    }

    if (skipEdgec2) {
      edges.remove('edgec2)
      neighborsOfCenterSeq = Seq('edgec1)
    }

    edges.values.foreach(metro += _)

    val neighborsOfOne = metro.getNeighbors(poiOne)
    val neighborsOfTwo = metro.getNeighbors(poiTwo)
    val neighborsOfCenter = metro.getNeighbors(cityCenter)

    Array('edge12, 'edge1c).foreach(symbol => assert(neighborsOfOne contains edges(symbol)))
    neighborsOfTwoSeq.foreach(symbol => assert(neighborsOfTwo contains edges(symbol)))
    neighborsOfCenterSeq.foreach(symbol => assert(neighborsOfCenter contains edges(symbol)))

    SampleGraph(metro, Seq(poiOne, poiTwo), edges.values.toSeq, cityCenter)
  }

  test("we can build a very simple metro graph") {
    assert(buildSimpleMetroGraph().graph.legal)
  }

  test("errors introduced into our simple metro graph are properly detected") {
    val graphData = buildSimpleMetroGraph()

    // Add a redundant edge
    graphData.graph += graphData.edges(0)
    assert(!graphData.graph.legal)
    assert(graphData.graph.infractions.exists(_ contains "#noredundantedges"))

    // Skip an edge to force bidirectional policy to fail
    val graphData2 = buildSimpleMetroGraph(false, true)
    assert(!graphData2.graph.legal)
    assert(graphData2.graph.infractions.exists(_ contains "#bidirectional"))

    // Skip two edges to force complete graph policy to fail
    val graphData3 = buildSimpleMetroGraph(true, true)
    assert(!graphData3.graph.legal)
    assert(graphData3.graph.infractions.exists(_ contains "#complete"))
  }
}
