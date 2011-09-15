/*
  Main.scala - Application
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.fatma

import java.io._
import java.nio._
import javax.swing._
import java.awt.{FlowLayout, BorderLayout, CardLayout, Event}
import java.awt.event._

import org.dmpp.os._
import org.dmpp.os.devices._

trait KeyboardEditorCommand {
  def execute: Unit
  def undo: Unit
}

trait KeyMapEditor {
  def executeCommand(command: KeyboardEditorCommand)
  def executeCommandNoUpdateUI(command: KeyboardEditorCommand)
  def undo
  def redo
  def keymapObject: KeyMapObject
  def selectedKey: Int
  def updateKeyboardView
}


object KeyboardEditor {
  val BaseTitle = "Fatma 1.0 - The Final Amiga Type Map Assembler"

  def readKeymapObject(inputStream: InputStream) = {
    var keymapObject: KeyMapObject = null
    try {
      val bytes = new Array[Byte](inputStream.available)
      inputStream.read(bytes)
      inputStream.close
      val hunk = new HunkFile(ByteBuffer.wrap(bytes))
      hunk.blocks.foreach(block => {
        block match {
          case codeBlock: CodeBlock =>
            keymapObject = KeymapReader.read(codeBlock)
          case _ => ;
        }
      })
    } catch {
      case e => e.printStackTrace
    }
    println("Keymap object is: " + keymapObject)
    keymapObject
  }
}

trait KeycodeSelectionMode {
  def keycodeSelected(keyCode: Int)
  def statusText: String
}

class KeyboardEditor(var keymapObject: KeyMapObject, isMacOs: Boolean)
extends JFrame(KeyboardEditor.BaseTitle) with KeyboardPanelListener
with KeyMapEditor {

  private var undoStack: List[KeyboardEditorCommand] = Nil
  private var redoStack: List[KeyboardEditorCommand] = Nil
  var selectedKey: Int = 0
  private var currentFile: File = null

  private var undoMenuItem: JMenuItem = null
  private var redoMenuItem: JMenuItem = null
  private val keyboardPanel = new KeyboardPanel(this)
  private val stringTablePanel = new StringTablePanel(this)
  private val deadDataPanel = new DeadDataPanel(this)
  private val flagsPanel = new FlagsPanel(this)
  private val mapValuesPanel = new MapValuesPanel(this)
  private val statusLabel = new JLabel("Ready")
  private val nopPanel = new JPanel
  nopPanel.setBorder(BorderFactory.createTitledBorder(
    BorderFactory.createEtchedBorder, "No mappings"))

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  keyboardPanel.addListener(this)
  updateTitleBar

  val centerPanel = new JPanel
  centerPanel.add(keyboardPanel)
  val bottomPanel = new JPanel(new BorderLayout)
  val cardLayout = new CardLayout
  val cardPanel = new JPanel(cardLayout)
  val statusPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  cardPanel.add(nopPanel, "nop")
  cardPanel.add(mapValuesPanel, "regular")
  cardPanel.add(stringTablePanel, "string")
  cardPanel.add(deadDataPanel, "dead")

  bottomPanel.add(cardPanel, BorderLayout.CENTER)
  bottomPanel.add(statusPanel, BorderLayout.SOUTH)
  statusPanel.add(statusLabel)
  bottomPanel.add(flagsPanel, BorderLayout.WEST)

  getContentPane.add(centerPanel, BorderLayout.CENTER)
  getContentPane.add(bottomPanel, BorderLayout.SOUTH)

  setupMenubar
  pack
  updateUI

  def executeCommand(command: KeyboardEditorCommand) {
    executeCommandNoUpdateUI(command)
    updateUI
  }
  def executeCommandNoUpdateUI(command: KeyboardEditorCommand) {
    command.execute
    undoStack ::= command
  }

  def undo {
    undoStack match {
      case command :: restOfStack =>
        command.undo
        undoStack = restOfStack
        redoStack ::= command
      case _ => ;
    }
    updateUI
  }

  def redo {
    redoStack match {
      case command :: restOfStack =>
        executeCommand(command)
        redoStack = restOfStack
      case _ => ;
    }
    updateUI
  }

  private def updateTitleBar {
    setTitle(KeyboardEditor.BaseTitle + " (Keymap: " + keymapObject.name + ")")
  }

  def setupMenubar {
    val menubar = new JMenuBar
    setJMenuBar(menubar)

    val fileMenu = new JMenu("File")
    val newMenuItem = new JMenuItem("New Keymap File...")
    setCtrlAccelerator(newMenuItem, KeyEvent.VK_N)
    val openMenuItem = new JMenuItem("Open Keymap File...")
    setCtrlAccelerator(openMenuItem, KeyEvent.VK_O)
    val saveAsMenuItem = new JMenuItem("Save as Keymap File...")
    setShiftCtrlAccelerator(saveAsMenuItem, KeyEvent.VK_S)
    val saveMenuItem = new JMenuItem("Save")
    setCtrlAccelerator(saveMenuItem, KeyEvent.VK_S)

    menubar.add(fileMenu)
    fileMenu.add(newMenuItem)
    fileMenu.add(openMenuItem)
    fileMenu.add(saveAsMenuItem)
    fileMenu.add(saveMenuItem)

    val editMenu = new JMenu("Edit")
    undoMenuItem = new JMenuItem("Undo")
    setCtrlAccelerator(undoMenuItem, KeyEvent.VK_Z)
    redoMenuItem = new JMenuItem("Redo")
    setShiftCtrlAccelerator(redoMenuItem, KeyEvent.VK_Z)
    menubar.add(editMenu)
    editMenu.add(undoMenuItem)
    editMenu.add(redoMenuItem)

    if (!isMacOs) {
      val quitMenuItem = new JMenuItem("Quit")
      setCtrlAccelerator(quitMenuItem, KeyEvent.VK_Q)
      fileMenu.addSeparator
      fileMenu.add(quitMenuItem)
    }

    saveAsMenuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) = saveAs
    })
    saveMenuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) = {
        if (currentFile == null) saveAs
        else saveCurrentFile
      }
    })

    openMenuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        val fileChooser = new JFileChooser
        if (fileChooser.showOpenDialog(KeyboardEditor.this) == JFileChooser.APPROVE_OPTION) {
          val selectedFile = fileChooser.getSelectedFile
          if (selectedFile.exists) {
            try {
              val inputStream = new FileInputStream(selectedFile)
              keymapObject = KeyboardEditor.readKeymapObject(inputStream)
              inputStream.close
              undoStack = Nil
              redoStack = Nil
              currentFile = selectedFile
              resetUI
            } catch {
              case e => e.printStackTrace
            }
          } else {
            // Show error
          }
        }
      }
    })

    undoMenuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) = undo
    })
    redoMenuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) = redo
    })
  }

  private def saveAs {
    val fileChooser = new JFileChooser
    if (fileChooser.showSaveDialog(KeyboardEditor.this) == JFileChooser.APPROVE_OPTION) {
      //TODO: confirm overwrite
      val canWrite = !fileChooser.getSelectedFile.exists ||
        JOptionPane.showConfirmDialog(this,
          "File '%s' exists. Overwrite ?".format(fileChooser.getSelectedFile.getName),
                                    "Confirm overwrite",
                                    JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION
      if (canWrite) {
        currentFile = fileChooser.getSelectedFile
        saveCurrentFile
        keymapObject.name = currentFile.getName
        updateTitleBar
      }
    }
  }

  private def saveCurrentFile {
    (new KeymapWriter(keymapObject)).write(currentFile)
    undoStack = Nil
    redoStack = Nil
    updateUI
  }

  private def resetUI {
    selectedKey = 0
    updateUI
    updateTitleBar
  }

  private def updateUI {
    flagsPanel.update
    deadDataPanel.update
    stringTablePanel.update
    mapValuesPanel.update
    keyboardPanel.update
    undoMenuItem.setEnabled(undoStack != Nil)
    redoMenuItem.setEnabled(redoStack != Nil)
    if (flagsPanel.nopRadio.isSelected) cardLayout.show(cardPanel, "nop")
    else if (flagsPanel.regularRadio.isSelected) cardLayout.show(cardPanel, "regular")
    else if (flagsPanel.stringRadio.isSelected) cardLayout.show(cardPanel, "string")
    else if (flagsPanel.deadRadio.isSelected) cardLayout.show(cardPanel, "dead")
  }

  def updateKeyboardView = keyboardPanel.update

  def setCtrlAccelerator(menuItem: JMenuItem, virtualKey: Int) {
    if (isMacOs) {
      menuItem.setAccelerator(KeyStroke.getKeyStroke(virtualKey, Event.META_MASK))
    } else {
      menuItem.setAccelerator(KeyStroke.getKeyStroke(virtualKey, Event.CTRL_MASK))
    }
  }
  def setShiftCtrlAccelerator(menuItem: JMenuItem, virtualKey: Int) {
    if (isMacOs) {
      menuItem.setAccelerator(KeyStroke.getKeyStroke(virtualKey,
                                                     Event.SHIFT_MASK | Event.META_MASK))
    } else {
      menuItem.setAccelerator(KeyStroke.getKeyStroke(virtualKey,
                                                     Event.SHIFT_MASK | Event.CTRL_MASK))
    }
  }

  def keycodeSelected(keyCode: Int) {
    selectedKey = keyCode
    updateUI
  }
}

object Main extends App {
  System.setProperty("apple.laf.useScreenMenuBar", "true")
  System.setProperty("com.apple.mrj.application.apple.menu.about.name",
                     "Fatma")
  val isMacOs = System.getProperty("os.name").toLowerCase.indexOf("mac") != -1
  if (!isMacOs) {
    UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel")
  }
  val KeyMapName = "usa1"
  //val KeyMapName = "d"

  val file = new File("Keymaps/" + KeyMapName)

  try {
    val inputStream = getClass.getClassLoader.getResourceAsStream("keymaps/" + KeyMapName)
    val keymapObject = KeyboardEditor.readKeymapObject(inputStream)
    val frame = new KeyboardEditor(keymapObject, isMacOs)
    frame.setVisible(true)
  } catch {
    case e => e.printStackTrace
  }
}
