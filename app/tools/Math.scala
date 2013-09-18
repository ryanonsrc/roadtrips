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

package io.nary.roadtrips.tools

/**
 * RoadTrips/Data :: HighwayNetwork
 *
 * Some Math "tools"
 */
object Math {
  // A very simple DSL for defining bounds
  def bounded(min: DisambiguatedBound, max: DisambiguatedBound, value: Double) : Boolean = max.upper(value) && min.lower(value)

  case class Bound(value: Double) {
    def INCLUSIVE : InclusiveBound = new InclusiveBound(value)
    def EXCLUSIVE : ExclusiveBound = new ExclusiveBound(value)
  }

  abstract class DisambiguatedBound { def upper(that: Double) : Boolean; def lower(that: Double) : Boolean }

  class InclusiveBound(value: Double) extends DisambiguatedBound {
    def upper(that: Double) = that <= value
    def lower(that: Double) = that >= value
  }

  class ExclusiveBound(value: Double) extends DisambiguatedBound {
    def upper(that: Double) = that < value
    def lower(that: Double) = that > value
  }
}