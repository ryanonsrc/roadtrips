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
package controllers

import data.source.HighwayNetworkBuilder

import play.api.mvc._
import play.api.mvc.Action
import core.regions.places.{LongitudeBounds, LatitudeBounds}

/**
 * RoadTrips/Controllers :: Data Services
 *
 * Entry-point for importing highway information and rendering it in Google Maps.
 */
object DataTools extends Controller {
  def showHighways = Action {
    // Build a Highway network with the following bounds and including only the following highways
    val network = new HighwayNetworkBuilder(LatitudeBounds(32.712739, 41.975283),
      LongitudeBounds(-124.716797, -113.972168), 16, 8, 1.0f).getNetwork("101", "99", "680", "580", "280", "880")

    network.fold // Automatically close the highway graph

    // Passes the highway data to the template for rendering via Google Maps
    Ok(views.html.data.highways(network.occupiedBucketVertices.flatMap(_._2).toSeq, network.occupiedBucketBoundaries.values.toSeq,
      network.allSegmentEdges, network.allFoldedEdges,
      network.islandAnnotatedDeadEnds, true))
  }
}
