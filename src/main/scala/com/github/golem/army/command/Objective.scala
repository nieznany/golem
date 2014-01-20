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
 * Sent, when actor want to defense some set of stones.
 *
 * @param nstones - how many stones is defended
 * @param nbreathsLeft - how many breaths CURRENTLY have given set of stones.
 */
case class Defense(nstones: Int, nbreathsLeft: Int) extends PrivatesObjective

/**
 * Sent, when an actor wants to have fun.
 */
case class Fun() extends Objective

/**
 * Sent, when an actor does not know what to do
 */
case class Despair() extends Objective
