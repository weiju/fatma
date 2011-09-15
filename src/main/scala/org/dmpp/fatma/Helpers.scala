/*
  Helpers.scala - common utilities to support the user interfaces
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.fatma
import org.dmpp.os.devices._

object CharConverter {
  def isPrintable(c: Int) = {
    (c >= 32 && c < 127) || (c >= 129 && c < 0x80) || c > 0x9f
  }
  def convertChar(c: Char): String = {
    val ch = c.asInstanceOf[Int] & 0xff
    if (isPrintable(ch)) {
      "%c".format(ch)
    } else ch match {
      case 0x00 => "<NUL>"
      case 0x01 => "<SOH>"
      case 0x02 => "<STX>"
      case 0x03 => "<ETX>"
      case 0x04 => "<EOT>"
      case 0x05 => "<ENQ>"
      case 0x06 => "<ACK>"
      case 0x07 => "<BEL>"
      case 0x08 => "<BS>"
      case 0x09 => "<TAB>"
      case 0x0a => "<LF>"
      case 0x0b => "<VT>"
      case 0x0c => "<FF>"
      case 0x0d => "<CR>"
      case 0x0e => "<$%02x>".format(ch)
      case 0x19 => "<EM>"
      case 0x1b => "<ESC>"
      case 0x7f => "<DEL>"
      case 0x8e => "<$%02x>".format(ch)
      case 0x9b => "<CSI>"
      case _ => "<$%02x>".format(ch)
    }    
  }
  def convertString(str: String): String = {
    val builder = new StringBuilder
    for (c <- str) builder.append(convertChar(c))
    builder.toString
  }
  def charsToHex(str: String): String = {
    val builder = new StringBuilder
    for (c <- str) builder.append("%02x".format(c.asInstanceOf[Int] & 0xff))
    builder.toString
  }
}

object ModifierTitles {
  val BaseTitles = Array("-", "Shift", "Alt", "Alt+Shift", "Ctrl",
                         "Ctrl+Shift", "Ctrl+Alt", "Ctrl+Alt+Shift")
  def titlesFor(flags: KeycodeFlags) = {
    var result = BaseTitles
    if (!flags.isShift) result = result.filter(!_.contains("Shift"))
    if (!flags.isAlt) result = result.filter(!_.contains("Alt"))
    if (!flags.isCtrl) result = result.filter(!_.contains("Ctrl"))
    result
  }
}
