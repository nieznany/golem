package com.github.golem.client.kgs

import java.util.Properties
import com.gokgs.client.gtp.{Options, GtpClient}
import java.io._
import scala.concurrent._
import com.github.golem.command.setup._

import com.github.golem.command.{EmptyResponse, CommandResponse, Command}
import ExecutionContext.Implicits.global
import akka.actor.{Props, Actor}
import scala.concurrent.duration.Duration
import com.github.golem.command.administrative.{ListCommands, GetVersion, GetName, StartClient}
import akka.event.Logging

object KgsClient {
  def props(properties: Properties, logFilename: String) = Props(classOf[KgsClient], properties, logFilename)

  def props(propertiesFileName: String, logFilename: String): Props = {
    val p = new Properties()
    p load new FileInputStream(propertiesFileName)
    KgsClient.props(p, logFilename)
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
class KgsClient(properties: Properties, logFilename: String) extends Actor {
  val NAME = "sagolem" // TODO get name and version from sbt build file
  val VERSION = "0.1"
  val AVAILABLE_COMMANDS = List()
  private val LOG = Logging(context.system, this)
  // We need to read command from output (gokgs gtp client writes to it).
  private val OUTPUT = new PipedOutputStream()
  private val OUTPUT_READER = new BufferedReader(new InputStreamReader(new PipedInputStream(OUTPUT)))
  // We need to write response to input (gokgs gtp client reads from it).
  private val INPUT = new PipedInputStream()
  private val INPUT_WRITER = new PrintWriter(new PipedOutputStream(INPUT))
  private val OPTIONS = new Options(properties, logFilename)

  var client : GtpClient = null

  def receive: Actor.Receive = {
    case StartClient => startClient()
    case m => LOG.warning(s"This message: $m is not handled by KgsClient")
  }

  def startClient() {
    val futureClient = future {
      new GtpClient(INPUT, OUTPUT, OPTIONS) // GtpClient constructor starts new thread, and next, blocks main thread. WTF?
      // Creation of new gtp client must be in separated thread - who knows why the hell
      // GtpClient calls blocking method once more. Calling GtpClient in 'main thread' will block it, so
      // we would not be able to read any command sent by 'GTP controller'.
    }
    // Almost as the 'Three-way Handshake'...
    getCommand // Ignoring result - fixed behaviour
    sendAvailableCommands
    getCommand
    sendName
    getCommand
    sendVersion

    val client: GtpClient = Await.result(futureClient, Duration.Inf)
    val goResult = future { // This result is currently not useful.
      client.go() // Blocking.
      LOG.info("Post client.go()")
    }

    // Now bot is sitting in selected room.
    while(true) {
      // This is called synchronously - assuring right order of handling commands with right responses.
      // TODO consider writing this part as asynchronous calls (recursive future creation). How to assure right order of pairs command/response?
      LOG.info("Waiting for next command...")
      val command = getCommand
      LOG.info(s"Got command: $command")
      try command match {
        case ListCommands => sendAvailableCommands
        case GetName => sendName
        case GetVersion => sendVersion
        case c => {LOG.info("Some message, which will be handled by commander"); sendResponse(EmptyResponse)}
      } catch {
        case e: UnsupportedOperationException => LOG.error(s"Unsupported operation: ${e.getMessage}")
        case e: Exception => LOG.error(s"Unknown exception: ${e.getMessage}")
      }
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
      case "boardsize" => SetBoardSize(arguments(1).toInt)
      case "komi" => SetKomi(arguments(1).toDouble)
      // Administrative
      case "list_commands" => ListCommands
      case "name" => GetName
      case "version" => GetVersion
      case u => {println("Unsupported"); throw new UnsupportedOperationException(u)}
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
  private def createKgsResponse(response: CommandResponse): String = {
    response match {
      case lcr: ListCommands.Response => {
        // Create appropriate list
        val buffer: StringBuffer = new StringBuffer
        lcr.commandsName.foreach(s => buffer.append(s + "\n"))
        createKgsResponse(buffer.toString.trim) // Removing last unnecessary newline sign.
      }
      case nr: GetName.Response => createKgsResponse(nr.name)
      case vr: GetVersion.Response => createKgsResponse(vr.version)
      case EmptyResponse => createKgsResponse("")

      case _ => throw new IllegalArgumentException(s"Unknown class of response: ${response.getClass}")
    }
  }

  /* ---------------------- Administrative commands handling ---------------------- */
  def sendAvailableCommands: Unit = sendResponse(new ListCommands.Response(AVAILABLE_COMMANDS))
  def sendName: Unit = sendResponse(new GetName.Response(NAME))
  def sendVersion: Unit = sendResponse(new GetVersion.Response(VERSION))


}
