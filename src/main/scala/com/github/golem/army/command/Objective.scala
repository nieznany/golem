package com.github.golem.army.command

trait Objective

trait PrivatesObjective extends Objective {
  def nstones: Int
  def nbreathsLeft: Int
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

/**
 * Sent when actor wants to stop opposite player from creating living fields
 * @param damage number of living fields opposite player would acquire if he made this move
 */
case class AttackGroup(damage:Int) extends Objective

/**
 * Sent when actor wants to create or expand living fields of group
 * @param gain of new living fields after the move
 */
case class DefendGroup(gain:Int) extends Objective
/**
 * Sent, when an actor wants to have fun.
 */
case class Fun() extends Objective

/**
 * Sent, when an actor does not know what to do
 */
case class Despair() extends Objective
