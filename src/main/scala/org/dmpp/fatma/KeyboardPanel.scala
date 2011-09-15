/*
  KeyboardPanel.scala - visual representation of a keyboard
  Created on 2011-09-02
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.fatma

import javax.swing._
import java.awt.{Graphics, Graphics2D, Color, Font, Dimension, RenderingHints}
import java.awt.event._
import org.dmpp.os.devices._

object KeyButton {
  val BorderRadius = 5
  val HollowAmigaIcon = new ImageIcon(getClass.getClassLoader.getResource("images/hollow_a_19x16.png"))
  val FilledAmigaIcon = new ImageIcon(getClass.getClassLoader.getResource("images/filled_a_19x16.png"))
  val ArrowUpIcon = new ImageIcon(getClass.getClassLoader.getResource("images/arrow_up_7x16.png"))
  val ArrowDownIcon = new ImageIcon(getClass.getClassLoader.getResource("images/arrow_down_7x16.png"))
  val ArrowRightIcon = new ImageIcon(getClass.getClassLoader.getResource("images/arrow_right_16x7.png"))
  val ArrowLeftIcon = new ImageIcon(getClass.getClassLoader.getResource("images/arrow_left_16x7.png"))
  val TabIcon = new ImageIcon(getClass.getClassLoader.getResource("images/tab_19x22.png"))
  val EnterIcon = new ImageIcon(getClass.getClassLoader.getResource("images/enter.png"))
  val ButtonFont1 = new Font("Times", Font.PLAIN, 10)
  val ButtonFont2 = new Font("Helvetica", Font.BOLD, 11)
  val ButtonFont3 = new Font("Helvetica", Font.BOLD, 9)
}

abstract class KeyButton(panel: KeyboardPanel, val keyCode: Int, var buttonWidth: Int,
                         var buttonHeight: Int, stringX: Int = 6) extends JButton {
  import KeyButton._
  setPreferredSize(new Dimension(buttonWidth, buttonHeight))
  setBackground(new Color(200, 200, 200))

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    drawLabel(g2d)
    g.setFont(ButtonFont1)
    g.setColor(Color.BLACK)
    g.drawString("%02x".format(keyCode), stringX, buttonHeight - 6)
  }

  protected def drawLabel(g: Graphics2D)
}
class MappedKeyButton(panel: KeyboardPanel, keyCode: Int, buttonWidth: Int,
                      buttonHeight: Int, stringX: Int = 6)
extends KeyButton(panel, keyCode, buttonWidth, buttonHeight, stringX) {
  protected def drawLabel(g: Graphics2D) {
    val x = 7
    val y = 16
    g.setFont(KeyButton.ButtonFont2)
    try {
      g.drawString(panel.keymapObject.unshiftedValue(keyCode), x, y)
    } catch {
      case e => e.printStackTrace
    }
  }
}

class EnterIconButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x44, buttonWidth, buttonHeight, 12) {
  override def drawLabel(g: Graphics2D) {
    g.drawImage(KeyButton.EnterIcon.getImage, 17, 24, null)
  }
}
class EnterTextButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x43, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    val x = 7
    val y = 16
    g.setFont(KeyButton.ButtonFont2)
    g.drawString("E", x, y)
    g.drawString("n", x, y + 9)
    g.drawString("t", x, y + 2 * 9)
    g.drawString("e", x, y + 3 * 9)
    g.drawString("r", x, y + 4 * 9)
  }
}
class CapsLockButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x62, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    val x = 7
    val y = 16
    g.setFont(KeyButton.ButtonFont3)
    g.drawString("Caps", x, y)
    g.drawString("Lock", x, y + 9)
    g.fillOval(30, 7, 5, 5)
  }
}
class LeftAmigaButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x66, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    g.drawImage(KeyButton.FilledAmigaIcon.getImage, 12, 7, null)
  }
}
class RightAmigaButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x67, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    g.drawImage(KeyButton.HollowAmigaIcon.getImage, 12, 7, null)
  }
}
class TabButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x42, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    g.drawImage(KeyButton.TabIcon.getImage, 10, 7, null)
  }
}
class ArrowUpButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x4c, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    g.drawImage(KeyButton.ArrowUpIcon.getImage, 10, 7, null)
  }
}
class ArrowDownButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x4d, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    g.drawImage(KeyButton.ArrowDownIcon.getImage, 10, 7, null)
  }
}
class ArrowRightButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int)
extends KeyButton(panel, 0x4e, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    g.drawImage(KeyButton.ArrowRightIcon.getImage, 10, 7, null)
  }
}
class ArrowLeftButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int,
                      keyCode: Int)
extends KeyButton(panel, keyCode, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    g.drawImage(KeyButton.ArrowLeftIcon.getImage, 7, 7, null)
  }
}

class StdTextButton(panel: KeyboardPanel, buttonWidth: Int, buttonHeight: Int,
                    keyCode: Int, label: String)
extends KeyButton(panel, keyCode, buttonWidth, buttonHeight) {
  override def drawLabel(g: Graphics2D) {
    val x = 7
    val y = 16
    g.setFont(KeyButton.ButtonFont2)
    g.drawString(label, x, y)
  }
}

trait KeyboardPanelListener {
  def keycodeSelected(keyCode: Int)
}

class KeyboardPanel(editor: KeyMapEditor) extends JPanel with ActionListener {
  setPreferredSize(new Dimension(920, 260))
  setLayout(null)
  val keyMap = new Array[KeyButton](128)
  var listeners: List[KeyboardPanelListener] = Nil

  makeFirstRow
  makeSecondRow
  makeThirdRow
  makeFourthRow

  addKey(new EnterIconButton(this, 60, 80),
         keyMap(0x0d).getX + 20,  keyMap(0x0d).getY + keyMap(0x0d).buttonHeight)

  makeFifthRow
  makeSixthRow

  def keymapObject = editor.keymapObject
  private def selectedKey = editor.selectedKey
  def addListener(l: KeyboardPanelListener) {
    listeners ::= l
  }

  private def makeFirstRow {
    val y = 10
    val width = 50
    addKey(new StdTextButton(this, 40, 40, 0x45, "Esc"), 10, y)

    // Function keys
    addKey(new StdTextButton(this, width, 40, 0x50, "F1"),
           keyMap(0x45).getX + keyMap(0x45).buttonWidth + 15, y)
    for (keyCode <- 0x51 to 0x54) {
      addKey(new StdTextButton(this, width, 40, keyCode, "F" + (keyCode - 0x4f)),
             keyMap(keyCode - 1).getX + keyMap(keyCode - 1).buttonWidth, y)
    }

    addKey(new StdTextButton(this, width, 40, 0x55, "F6"),
           keyMap(0x54).getX + keyMap(0x54).buttonWidth + 25, y)
    for (keyCode <- 0x56 to 0x59) {
      addKey(new StdTextButton(this, width, 40, keyCode, "F" + (keyCode - 0x4f)),
             keyMap(keyCode - 1).getX + keyMap(keyCode - 1).buttonWidth, y)
    }
  }

  private def makeSecondRow {
    val y = keyMap(0x45).getY + keyMap(0x45).buttonHeight + 8
    addKey(0x00, 10, y, 50)
    addKey(0x01, keyMap(0x00).getX + keyMap(0x00).buttonWidth, y)
    for (i <- 2 to 13) {
      addKey(i, keyMap(i - 1).getX + keyMap(i - 1).buttonWidth, y)
    }
    addKey(new ArrowLeftButton(this, 40, 40, 0x41),
            keyMap(0x0d).getX + keyMap(0x0d).buttonWidth, y)

    addKey(new StdTextButton(this, 50, 40, 0x46, "Del"),
           keyMap(0x41).getX + keyMap(0x41).buttonWidth + 10, y)
    addKey(new StdTextButton(this, 50, 40, 0x5f, "Help"),
           keyMap(0x46).getX + keyMap(0x46).buttonWidth, y)

    addKey(0x5a, keyMap(0x5f).getX + keyMap(0x5f).buttonWidth + 10, y)

    for (i <- 0x5b to 0x5d) {
      addKey(i, keyMap(i - 1).getX + keyMap(i - 1).buttonWidth, y)
    }
  }

  private def makeThirdRow {
    val y = keyMap(0x00).getY + keyMap(0x00).buttonHeight
    
    addKey(new TabButton(this, 70, 40), 10, y)
    addKey(0x10, keyMap(0x42).getX + keyMap(0x42).buttonWidth, y)
    for (i <- 0x11 to 0x1b) {
      addKey(i, keyMap(i - 1).getX + keyMap(i - 1).buttonWidth, y)
    }
    addKey(0x3d, keyMap(0x5a).getX, y)
    addKey(0x3e, keyMap(0x5b).getX, y)
    addKey(0x3f, keyMap(0x5c).getX, y)
    addKey(0x4a, keyMap(0x5d).getX, y)
  }

  private def makeFourthRow {
    val y = keyMap(0x42).getY + keyMap(0x42).buttonHeight
    addKey(new StdTextButton(this, 40, 40, 0x63, "Ctrl"), 10, y)
    addKey(new CapsLockButton(this, 40, 40),
           keyMap(0x63).getX + keyMap(0x63).buttonWidth, y)

    addKey(0x20, keyMap(0x62).getX + keyMap(0x62).buttonWidth, y)
    for (i <- 0x21 to 0x2b) {
      addKey(i, keyMap(i - 1).getX + keyMap(i - 1).buttonWidth, y)
    }

    addKey(new ArrowUpButton(this, 40, 40),
           keyMap(0x2b).getX + keyMap(0x2b).buttonWidth + 90, y)

    addKey(0x2d, keyMap(0x5a).getX, y)
    addKey(0x2e, keyMap(0x5b).getX, y)
    addKey(0x2f, keyMap(0x5c).getX, y)
    addKey(0x5e, keyMap(0x5d).getX, y)
  }

  private def makeFifthRow {
    val y = keyMap(0x63).getY + keyMap(0x63).buttonHeight
    addKey(new StdTextButton(this, 70, 40, 0x60, "Shift"), 10, y)

    addKey(0x30, keyMap(0x60).getX + keyMap(0x60).buttonWidth, y)
    for (i <- 0x31 to 0x3a) {
      addKey(i, keyMap(i - 1).getX + keyMap(i - 1).buttonWidth, y)
    }
    addKey(new StdTextButton(this, 100, 40, 0x61, "Shift"),
           keyMap(0x3a).getX + keyMap(0x3a).buttonWidth, y)

    addKey(new ArrowDownButton(this, 40, 40), keyMap(0x4c).getX, y)
    addKey(new ArrowLeftButton(this, 40, 40, 0x4f),
            keyMap(0x61).getX + keyMap(0x61).buttonWidth, y)
    addKey(new ArrowRightButton(this, 40, 40),
           keyMap(0x4d).getX + keyMap(0x4d).buttonWidth, y)

    addKey(0x1d, keyMap(0x5a).getX, y)
    addKey(0x1e, keyMap(0x5b).getX, y)
    addKey(0x1f, keyMap(0x5c).getX, y)
  }

  private def makeSixthRow {
    val y = keyMap(0x60).getY + keyMap(0x60).buttonHeight
    addKey(new StdTextButton(this, 50, 40, 0x64, "Alt"), 70, y)
    addKey(new LeftAmigaButton(this, 50, 40),
           keyMap(0x64).getX + keyMap(0x64).buttonWidth, y)

    addKey(0x40, keyMap(0x66).getX + keyMap(0x66).buttonWidth, y, 300)

    addKey(new RightAmigaButton(this, 50, 40),
           keyMap(0x40).getX + keyMap(0x40).buttonWidth, y)

    addKey(new StdTextButton(this, 50, 40, 0x65, "Alt"),
           keyMap(0x67).getX + keyMap(0x67).buttonWidth, y)

    addKey(0x0f, keyMap(0x5a).getX, y, 80)
    addKey(0x3c, keyMap(0x5c).getX, y)
    // Numpad Enter
    addKey(new EnterTextButton(this, 40, 80),
           keyMap(0x5d).getX, y - 40)
  }

  def addKey(button: KeyButton, x: Int, y: Int) {
    if (keyMap.contains(button.keyCode)) {
      throw new IllegalArgumentException("key code %d already used.".format(button.keyCode))
    }
    button.addActionListener(this)
    keyMap(button.keyCode) = button
    add(button)
    button.setBounds(x, y, button.buttonWidth, button.buttonHeight)
  }

  def addKey(keyCode: Int, x: Int, y: Int, width: Int = 40, height: Int = 40,
             labelX: Int = 6) {
    if (keyMap.contains(keyCode)) {
      throw new IllegalArgumentException("key code %d already used.".format(keyCode))
    }
    val button =  new MappedKeyButton(this, keyCode, width, height, labelX)
    button.addActionListener(this)
    keyMap(keyCode) = button
    add(button)
    button.setBounds(x, y, button.buttonWidth, button.buttonHeight)
  }

  def actionPerformed(e: ActionEvent) {
    val button = e.getSource.asInstanceOf[KeyButton]
    listeners.foreach(l => l.keycodeSelected(button.keyCode))
  }

  def update {
    keyMap(selectedKey).requestFocusInWindow
  }
}
