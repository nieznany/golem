package com.github.golem.army.command

trait Objective

trait PrivatesObjective extends Objective {
  def nstones: Int
  def nbreathsLeft: Int
}

trait CaptainsObjective extends Objective {
  def nstones: Int
}

/**
 * Sent, when actor want to kill some set of stones.
 *
 * @param nstones - how many stones is attacked
 * @param nbreathsLeft - how many breaths CURRENTLY have given set of stones.
 */
case class Attack(nstones: Int, nbreathsLeft: Int) extends PrivatesObjective

/**
 * Sent, when actor want to defend some set of stones.
 *
 * @param nstones - how many stones is defended
 * @param nbreathsLeft - how many breaths CURRENTLY have given set of stones.
 */
case class Defense(nstones: Int, nbreathsLeft: Int) extends PrivatesObjective

case class Death(nstones: Int) extends PrivatesObjective {
  def nbreathsLeft: Int = 1
}

/**
 * Sent when actor wants to stop opposite player from creating living fields
 * @param nstones number of living fields opposite player would acquire if he made this move
 */
case class AttackGroup(nstones:Int) extends CaptainsObjective

/**
 * Sent when actor wants to create or expand living fields of group
 * @param nstones of new living fields after the move
 */
case class DefendGroup(nstones:Int) extends CaptainsObjective
/**
 * Sent, when an actor wants to have fun.
 */
case class Fun() extends Objective

/**
 * Sent, when an actor does not know what to do
 */
case class Despair() extends Objective

/**
 * Commander's objective - to explore board on start
 */
case class Exploration() extends Objective
