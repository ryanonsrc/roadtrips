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
package source

import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.jdbc.{StaticQuery => Q}
import collection.mutable.ListBuffer
import core.regions.places.{LongitudeBounds, LatitudeBounds, UnnamedPoint}
import data.common.HighwayNetwork

/**
 * RoadTrips/Data :: HighwayNetworkBuilder
 *
 * Reads in OpenStreetMap.org data from the postgres database.
 */
class HighwayNetworkBuilder(latitudeBounds: LatitudeBounds, longitudeBounds: LongitudeBounds, lateralResolution: Int, longitudinalResolution: Int, stepSize: Float) {
  private case class Road (id: Long, highway: String, ref: String, geometry: String)

  private val interpolationFactors : Seq[Float] = {
    val list = new ListBuffer[Float]()
    var value = 0.0f
    list += value
    while ((value + stepSize) <= 1) {
      value += stepSize
      list += value
    }

    list
  }

  def getNetwork : HighwayNetwork = getNetwork()
  def getNetwork(highwayWhiteList : String*) : HighwayNetwork = {
    val highwayNetwork = new HighwayNetwork(latitudeBounds, longitudeBounds, lateralResolution, longitudinalResolution)
    val highwayFilter = highwayWhiteList match {
      case whiteList if whiteList.isEmpty => ""
      case whiteList => whiteList.map(highway => s""""ref" LIKE '%${highway}%'""").mkString("and (", " or ", ")")
    }

    Database.forURL(url = "jdbc:postgresql:gis", user = "postgres", password = "postgres", driver = "org.postgresql.Driver") withSession {

      val highways = Q.queryNA[String](
        s"""select "ref" from planet_osm_roads where highway LIKE '%motorway%' ${highwayFilter} group by "ref" """
      )

      highways.list().foreach { highway =>
        val segments = Q.query[String, String](
          """select way from planet_osm_roads where highway LIKE '%motorway%' and "ref" = ?"""
        )

        segments.list(highway).foreach( segment => {
          interpolationFactors.foreach( factor => {
            val pointsAlongPart = Q.query[(String, Float), (Double, Double)](
                """
                  |select ST_Y(point) as latitude, ST_X(point) as longitude from (
                  |	select ST_AsText(
                  |		Geography(ST_Transform(ST_Line_Interpolate_Point(ST_GeomFromText(?), ?), 4326))
                  |	) as point
                  |) as transform
                """.stripMargin)

            pointsAlongPart.list(segment, factor).foreach{ case (k, v) => highwayNetwork += new UnnamedPoint(k, v) }
          })

          highwayNetwork.endSegment
        })

        highwayNetwork.endHighway(highway)
      }

      highwayNetwork
    }
  }
}
