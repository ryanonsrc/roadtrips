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
package source.json

import collection.immutable.PagedSeq
import util.parsing.input.PagedSeqReader
import play.libs.Json
import org.codehaus.jackson.JsonNode


/**
 * RoadTrips/Data :: PlaceFileReader
 *
 * Reader capable of handling very large JSON files contain all places and routes.
 * Note: This data source reader is used primarily for testing purposes.
 */
object PlaceFileReader {

  private val placesKey = "\"places\""
  private val routesKey = "\"places\""
  private val clearAndStop = ']'

  // TODO: Change this to file property
  private def fileContent = new PagedSeqReader(PagedSeq.fromFile("/var/tmp/some-places.json"))

  private def takeUntil(readerInitial: PagedSeqReader, foundCondition: String => Boolean, mapText: String => String = (noChange => noChange)) : Taken = {
    val str = new StringBuilder()
    var readerFinal = readerInitial

    while(!readerFinal.atEnd && !foundCondition(str.toString)) {
      str += readerFinal.first
      readerFinal = readerFinal.rest
    }

    if (str.contains(clearAndStop))
      Taken(readerFinal, None)
    else if (foundCondition(str.toString))
      Taken(readerFinal, Some(mapText(str.toString)))
    else
      Taken(readerFinal, None)
  }

  private def takeUntil(readerInitial: PagedSeqReader, chars: Char*) : Taken = {
    var taken = Taken(readerInitial, None)
    chars.foreach(ch => taken = takeUntil(taken.reader, _.endsWith(ch + "")))
    taken
  }

  private def takeAllUntil(readerInitial: PagedSeqReader, chars: Char*) : Taken = {
    var taken = Taken(readerInitial, None)
    chars.foreach(ch => taken = taken augment takeUntil(taken.reader, _.endsWith(ch + "")))
    taken
  }

  private def takeUntilListItems() = takeUntil(fileContent, str => str.endsWith(placesKey) || str.endsWith(routesKey), str =>
    if (str.endsWith(placesKey)) placesKey
    else if(str.endsWith(routesKey)) routesKey
    else ""
  )

  def processJsonData(processPlace: JsonNode => Unit, processRoute: JsonNode => Unit) : Unit = {
    var taken = takeUntilListItems()

    for (i <- 1 to 2) {
      if (taken.finishedFirstPass || (taken.text != None && taken.text.get != "")) {
        val handlePlaces = taken.text.get == placesKey
        taken = takeUntil(taken.reader, ':', '[')

        var doneFirst = false
        while (taken.text != None) {
          if (!doneFirst)
            doneFirst = true
          else
            taken = takeUntil(taken.reader, ',')

          if (taken.text != None)
            taken = handlePlaces match {
              case true => takeUntil(taken.reader, '}')
              case false => takeAllUntil(taken.reader, '}', '}', '}')
            }

          if (taken.text != None) {
            handlePlaces match {
              case true => processPlace(Json.parse(taken.text.get))
              case false => processRoute(Json.parse(taken.text.get))
            }
          }
        }
      }
      taken = taken.nextPass
    }
  }

}
case class Taken(reader: PagedSeqReader, text: Option[String], finishedFirstPass: Boolean = false) {
  def nextPass = Taken(reader, Some(""), true)
  def augment(next: Taken) = Taken(next.reader, Some((text match {case None => "" case v: Some[String] => v.get }) + next.text.get), finishedFirstPass)
}