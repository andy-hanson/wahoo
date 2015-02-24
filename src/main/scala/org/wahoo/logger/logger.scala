package org.wahoo
package logger

import java.io.{BufferedWriter, File, FileWriter}

/** Writes to log files.
  * Traits inheriting the Logs interface make calls to this object.
  * MooMain must call logger.start at startup and logger.updateTime every frame.
  *
  * Objects of trait Logs specify a category when logging.
  * These categories are used to make separate log files for separate subjects.
  * A complete log is also made.
  */

case class LogCategoryNotRecognizedException(category:Symbol) extends Exception {
  //override def toString = "LogCategoryNotRecognizedException("+category.name+")"
}

object Logger {

  /** Whether any logging will occur at all. */
  var loggingOn = false
  /** Name of the directory to put logs.
    * It does not have to exist prior to running.
    */
  val Directory = "logs"
  /** Name of the file to place the full log. */
  val LogAllFileName = Directory + "/all"

  /** Every symbol that represents a type of thing to log. */
  var allCategories: Set[Symbol] = null
  /** The things that will be logged in this run (if loggingOn is true). */
  var onCategories: Set[Symbol] = null

  /** Time in frames. Must be updated by calls to updateTime(). */
  var time = 0
  /** Call this function once per frame to get the logger to update. */
  def updateTime() { time += 1 }

  /** Whether a category is being logged. */
  def isCategoryOn(category:Symbol) = onCategories.contains(category)
  /** Name of the log file for some category. */
  def logFileFor(category:Symbol) = Directory + "/" + category.name

  /** Tell the logger whether and what to log.
    * @param on Whether the logger does anything at all.
    * @param fullListOfCategories Every possible category to log.
    * @param onCategories Categories to log this runthrough.
    */
  def start(on:Boolean,
            fullListOfCategories:Traversable[Symbol],
            loggedCategories:Traversable[Symbol]) {
    if (loggingOn) throw new RuntimeException("Logger started twice!")

    if (on) {
      allCategories = fullListOfCategories.toSet
      onCategories  = loggedCategories.toSet

      //Throw an error if a nonexistent category is turned on.
      onCategories.foreach { cat =>
        if (!allCategories.contains(cat))
          throw LogCategoryNotRecognizedException(cat)
      }
      assert(onCategories.subsetOf(allCategories))
      resetFiles()

      loggingOn = true
    }
  }

  /** Delete the directory of name dirName.
    * Does nothing if it's not there anyway.
    * Fails if there are other directories inside.
    */
  private def deleteDirectory(dirName:String) {
    var dir = new File(dirName)
    if( dir.exists() )
      dir.listFiles().foreach { file =>
        if (file.isDirectory)
          throw new RuntimeException("Unexpected inner directory in "+dirName)
        file.delete()
      }
  }

  /** Delete the old logs and make new log files. */
  def resetFiles() {
    deleteDirectory(Directory)
    new File(Directory).mkdir()
    new File(LogAllFileName).createNewFile()
    onCategories.foreach { cat => new File(logFileFor(cat)).createNewFile() }
  }


  /** Write a log.
    * @param category Type of thing this log applies to.
    * @param subject Object that called this log.
    * @param description What the log is about.
    */
  def log(category:Symbol, subject:Any, description: => Any) {
    if (loggingOn) {
      if (!allCategories.contains(category))
        throw new LogCategoryNotRecognizedException(category)

      if (isCategoryOn(category)) {
        //Get the stack trace 2 calls up.
        //This call is call 0, Logs#log is call 1.
        val stackTrace = new Exception().getStackTrace()(2).toString
        val log = logString(stackTrace, subject, description)
        appendLogTo(logFileFor(category), log)
        appendLogTo(LogAllFileName, log)
      }
    }
  }

  /** Format a string for a single log. */
  def logString(stackTrace:String, subject:Any, description:Any) =
    "%48s  %6d  %32s    %s".format(stackTrace, time, subject.toString, description)

  /** Put a log at the end of the file. */
  def appendLogTo(fileName:String, log:String) {
    val writer = new BufferedWriter(new FileWriter(fileName, true))
    writer.write(log)
    writer.newLine()
    writer.flush()
    writer.close()
  }
}
