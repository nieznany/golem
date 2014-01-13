package com.github.golem.command.administrative

import com.github.golem.command.Informative

/**
 *  Should be sent to {@link com.github.golem.client.kgs.KgsClient} instance
 *  when we want to start bot.
 */
object StartClient extends AdministrativeCommand with Informative
