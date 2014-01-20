package com.github.golem.army.model

import akka.actor.ActorRef
import com.github.golem.model.Board.Stone
import com.github.golem.model.BasicRulesGame.{Group, Chain}
import com.github.golem.model.Board.Coords

/**
 * Bidirectional map for subordinate <-> one of positions (stones) on board
 */
case class Subordinates(private val referenceStones: Map[ActorRef, Stone] = Map[ActorRef, Stone](),
                        private val subordinatesMap: Map[Coords, ActorRef] = Map[Coords, ActorRef]()) {

  def getActors: Set[ActorRef] = referenceStones.keySet

  def getReferenceStoneFor(actor: ActorRef): Stone = referenceStones(actor)

  def getSubordinateFor(coords: Coords): Option[ActorRef] = subordinatesMap.get(coords)

  def addGroups(ags: Iterable[(ActorRef, Group)]): Subordinates = {
    var acs = List[(ActorRef, Chain)]()
    for(ag <- ags) {
      for(chain <- ag._2.chains) {
        acs = (ag._1, chain) :: acs
      }
    }
    this + acs
  }

  def getCoordsForSubordinate(actor: ActorRef): Set[Coords] = {
    val subordinateCoords = scala.collection.mutable.Set[Coords]()
    for(kv <- subordinatesMap) {
      if(kv._2 == actor)
        subordinateCoords += kv._1
    }
    subordinateCoords.toSet
  }

  def +(acs: Iterable[(ActorRef, Chain)]): Subordinates = {
    var newRefPositions = referenceStones
    var newSubMap = subordinatesMap
    for (ac <- acs) {
      newRefPositions += (ac._1 -> ac._2.fields.head)
      newSubMap ++= (for (stone <- ac._2.fields) yield (stone.position -> ac._1))
    }
    Subordinates(newRefPositions, newSubMap)
  }

  def -(actor: ActorRef): Subordinates = {
    this - Set[ActorRef](actor)
  }

  def -(actors: Set[ActorRef]): Subordinates = {
    var newSubMap = subordinatesMap
    var newRefPositions = referenceStones
    for (kv <- subordinatesMap) {
      // FIX very inefficient, but most general (it will actually remove all actor's stones) - what to do with this method?
      if (actors.contains(kv._2))
        newSubMap -= kv._1
    }
    for (actor <- actors)
      newRefPositions -= actor
    Subordinates(newRefPositions, newSubMap)
  }

}
