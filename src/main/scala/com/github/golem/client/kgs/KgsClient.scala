package com.github.golem.client.kgs

import java.util.Properties
import com.gokgs.client.gtp.{Options, GtpClient}
import java.io._
import java.util.concurrent.{Executors, Callable, FutureTask}
import com.github.golem.client.kgs.command._
import com.github.golem.command.setup._

import com.github.golem.command.{CommandResponse, Command}

object KgsClient {
  def apply(properties: Properties, logFilename: String) = new KgsClient(properties, logFilename)

  def apply(propertiesFileName: String, logFilename: String): KgsClient = {
    val p = new Properties()
    p load new FileInputStream(propertiesFileName)
    KgsClient(p, logFilename)
  }
}

/**
 * KGS GTP (Go Text Protocol) scala wrapper.
 * <br/><br/>
 * NOTE: Kgs gtp developer was evil:
 * he has left no documentation and no source code. And that damn
 * library (kgsGtp.jar) has strange behaviour, see comments below for details.
 * So if you want to change something in below class, please, be careful!
 */
class KgsClient(properties: Properties, logFilename: String) {
  val NAME = "sagolem" // TODO get name and version from sbt build file
  val VERSION = "0.1"
  val AVAILABLE_COMMANDS = List()

  // We need to read command from output (gokgs gtp client writes to it).
  private val OUTPUT = new PipedOutputStream()
  private val OUTPUT_READER = new BufferedReader(new InputStreamReader(new PipedInputStream(OUTPUT)))

  // We need to write response to input (gokgs gtp client reads from it).
  private val INPUT = new PipedInputStream()
  private val INPUT_WRITER = new PrintWriter(new PipedOutputStream(INPUT))

  private val OPTIONS = new Options(properties, logFilename)
  var CLIENT : GtpClient = null

  def start() {
    val futureClient = new FutureTask[GtpClient](new Callable[GtpClient] {
      def call() = new GtpClient(INPUT, OUTPUT, OPTIONS) // GtpClient constructor starts new thread, and next, blocks main thread. WTF?
      // Creation of new gtp client must be in separated thread - who knows why the hell
      // GtpClient calls blocking method once more. Calling GtpClient in 'main thread' will block it, so
      // we would not be able to read any command sent by 'GTP controller'.
    })
    Executors.newSingleThreadExecutor().execute(futureClient)

    // Almost as the 'Three-way Handshake'...
    getCommand // Ignoring result - fixed behaviour
    sendResponse(new ListCommands.Response(AVAILABLE_COMMANDS)) // TODO send real list of supported commands
    getCommand
    sendResponse(new GetName.Response(NAME))
    getCommand
    sendResponse(new GetVersion.Response(VERSION))

    futureClient.get().go()
    // Now bot is sitting in selected room.
    while(true) {
      val command = getCommand
//      command match {
//
//      }
    }
  }

  /* ---------------------- Command handling ---------------------- */

  /**
   * Deserialize object from gtp controller message.
   * // TODO actually, this could be implemented as a deserializer.
   * @return deserialized command
   */
  private def getCommand: Command = {
    val commandString: String = OUTPUT_READER readLine() // Blocks current thread.
    val arguments = commandString.trim.split(" ")

    // KGS GTP: assume, that element 0 is command name, 1...n - arguments
    val commandName = arguments(0)

    commandName match {
      case "board_size" => SetBoardSize(arguments(1).toInt)

      case "komi" => SetKomi(arguments(1).toDouble)
    }
  }

  /* ---------------------- Response handling ---------------------- */
  private def sendResponse(response: CommandResponse):Unit = {
    INPUT_WRITER write createKgsResponse(response)
    INPUT_WRITER flush()
  }

  private val RESPONSE_PATTERN = "= %response%\n\n"
  private def createKgsResponse(response: String) = RESPONSE_PATTERN.replace("%response%", response)

  /**
   * Converts response to appropriate kgs string response.
   * Each of response string is compatible with specification of GTP's responses.
   * See <a href="http://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html#SECTION00044000000000000000">this site</a>
   *  for further details.
   *
   *  TODO actually, this could be implemented as a special case of serializer.
   *
   * @param response, which should be converted.
   * @return kgs string response
   * @throws IllegalArgumentException when response type is unknown
   */
  def createKgsResponse(response: CommandResponse): String = {
    response match {

      case lcr: ListCommands.Response => {
        // Create appropriate list
        val buffer: StringBuffer = new StringBuffer
        lcr.commandsName.foreach(s => buffer.append(s + "\n"))
        createKgsResponse(buffer.toString.trim) // Removing last unnecessary newline sign.
      }

      case nr: GetName.Response => createKgsResponse(nr.name)

      case vr: GetVersion.Response => createKgsResponse(vr.version)

      case _ => throw new IllegalArgumentException("Unknown class of response: " + response.getClass)
    }
  }
}
