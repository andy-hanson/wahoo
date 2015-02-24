package org.wahoo
package logger

trait Logs {
  def log(category:Symbol, description: => Any) {
    Logger.log(category, this, description)
  }
}
