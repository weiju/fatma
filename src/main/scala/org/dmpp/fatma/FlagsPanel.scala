/*
  FlagsPanel.scala - keycode flags panel
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.fatma

import javax.swing._
import java.awt.{GridLayout, FlowLayout}
import org.dmpp.os.devices._
import java.awt.event._

class FlagsPanel(editor: KeyMapEditor) extends Box(BoxLayout.Y_AXIS) {
  val typePanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  val buttonGroup = new ButtonGroup
  val nopRadio = new JRadioButton("NOP")
  val regularRadio = new JRadioButton("Regular")
  val stringRadio = new JRadioButton("String")
  val deadRadio = new JRadioButton("Dead")

  val flagsPanel = new JPanel(new GridLayout(0, 3))
  val shiftCheckBox = new JCheckBox("Shift")
  val altCheckBox = new JCheckBox("Alt")
  val ctrlCheckBox = new JCheckBox("Ctrl")
  val repeatCheckBox = new JCheckBox("Repeat")
  val capsCheckBox = new JCheckBox("Caps ")
  val downUpCheckBox = new JCheckBox("DownUp")

  add(typePanel)
  add(new JSeparator)
  add(flagsPanel)
  setBorder(BorderFactory.createTitledBorder(
    BorderFactory.createEtchedBorder, "Flags"))

  buttonGroup.add(regularRadio)
  buttonGroup.add(nopRadio)
  buttonGroup.add(stringRadio)
  buttonGroup.add(deadRadio)
  typePanel.add(nopRadio)
  typePanel.add(regularRadio)
  typePanel.add(stringRadio)
  typePanel.add(deadRadio)

  flagsPanel.add(shiftCheckBox)
  flagsPanel.add(altCheckBox)
  flagsPanel.add(ctrlCheckBox)
  flagsPanel.add(capsCheckBox)
  flagsPanel.add(repeatCheckBox)
  flagsPanel.add(new JLabel("")) // placeholder
  flagsPanel.add(downUpCheckBox)
  flagsPanel.add(new JLabel("")) // placeholder
  flagsPanel.add(new JLabel("")) // placeholder

  // Connect actions
  val radioListener = new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val command = new KeyboardEditorCommand {
        val originalKey = selectedKey
        val originalFlags = editor.keymapObject.flagsFor(selectedKey)
        
        def execute {
          if (nopRadio.isSelected) keymapObject.setIsNop(selectedKey)
          else if (regularRadio.isSelected) keymapObject.setIsRegular(selectedKey)
          else if (stringRadio.isSelected) keymapObject.setIsString(selectedKey)
          else if (deadRadio.isSelected) keymapObject.setIsDead(selectedKey)
        }
        def undo {
          editor.keymapObject.setFlagsFor(selectedKey, originalFlags)
        }
      }
      editor.executeCommand(command)
    }
  }
  regularRadio.addActionListener(radioListener)
  nopRadio.addActionListener(radioListener)
  stringRadio.addActionListener(radioListener)
  deadRadio.addActionListener(radioListener)

  shiftCheckBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val command = new KeyboardEditorCommand {
        val originalKey = selectedKey
        val isSelected = shiftCheckBox.isSelected
        def execute =
          editor.keymapObject.setIsShift(selectedKey, isSelected)
        def undo =
          editor.keymapObject.setIsShift(originalKey, !isSelected)
      }
      editor.executeCommand(command)
    }
  })
  altCheckBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val command = new KeyboardEditorCommand {
        val originalKey = selectedKey
        val isSelected = altCheckBox.isSelected
        def execute =
          editor.keymapObject.setIsAlt(selectedKey, isSelected)
        def undo =
          editor.keymapObject.setIsAlt(originalKey, !isSelected)
      }
      editor.executeCommand(command)
    }
  })
  ctrlCheckBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val command = new KeyboardEditorCommand {
        val originalKey = selectedKey
        val isSelected = ctrlCheckBox.isSelected
        def execute =
          editor.keymapObject.setIsCtrl(selectedKey, isSelected)
        def undo =
          editor.keymapObject.setIsCtrl(originalKey, !isSelected)
      }
      editor.executeCommand(command)
    }
  })
  capsCheckBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val command = new KeyboardEditorCommand {
        val originalKey = selectedKey
        val isSelected = capsCheckBox.isSelected
        def execute =
          editor.keymapObject.setIsCapsable(selectedKey, isSelected)
        def undo =
          editor.keymapObject.setIsCapsable(originalKey, !isSelected)
      }
      editor.executeCommand(command)
    }
  })
  repeatCheckBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val command = new KeyboardEditorCommand {
        val originalKey = selectedKey
        val isSelected = repeatCheckBox.isSelected
        def execute =
          editor.keymapObject.setIsRepeatable(selectedKey, isSelected)
        def undo =
          editor.keymapObject.setIsRepeatable(originalKey, !isSelected)
      }
      editor.executeCommand(command)
    }
  })
  downUpCheckBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val command = new KeyboardEditorCommand {
        val originalKey = selectedKey
        val isSelected = downUpCheckBox.isSelected
        def execute =
          editor.keymapObject.setIsDownUp(selectedKey, isSelected)
        def undo =
          editor.keymapObject.setIsDownUp(originalKey, !isSelected)
      }
      editor.executeCommand(command)
    }
  })

  private def keymapObject = editor.keymapObject
  private def selectedKey = editor.selectedKey

  def update {
    clearFlags
    if (keymapObject.isNop(selectedKey)) {
      nopRadio.setSelected(true)      
      enableControls(false)
    } else {
      enableControls(true)
      if (keymapObject.isString(selectedKey)) stringRadio.setSelected(true)
      else if (keymapObject.isDead(selectedKey)) deadRadio.setSelected(true)
      else regularRadio.setSelected(true)
      shiftCheckBox.setSelected(keymapObject.isShift(selectedKey))
      altCheckBox.setSelected(keymapObject.isAlt(selectedKey))
      ctrlCheckBox.setSelected(keymapObject.isCtrl(selectedKey))
      capsCheckBox.setSelected(keymapObject.isCapsable(selectedKey))
      repeatCheckBox.setSelected(keymapObject.isRepeatable(selectedKey))
      downUpCheckBox.setSelected(keymapObject.isDownUp(selectedKey))
    }
  }

  private def clearFlags {
    shiftCheckBox.setSelected(false)
    altCheckBox.setSelected(false)
    ctrlCheckBox.setSelected(false)
    capsCheckBox.setSelected(false)
    repeatCheckBox.setSelected(false)
    downUpCheckBox.setSelected(false)
  }

  private def enableControls(flag: Boolean) {
    shiftCheckBox.setEnabled(flag)
    altCheckBox.setEnabled(flag)
    ctrlCheckBox.setEnabled(flag)
    capsCheckBox.setEnabled(flag)
    repeatCheckBox.setEnabled(flag)
    downUpCheckBox.setEnabled(flag)    
  }
}
