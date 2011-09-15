/*
  Exec.scala - Exec data structures
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.os

class Node(var lnSucc: Int, var lnPred: Int, var lnType: Int,
           var lnPri: Int, var lnName: Int) {
  override def toString = {
    ("Node(succ = $%04x, pred = $%04x, type = %d, pri = %d, " +
    "name = %d)").format(lnSucc, lnPred, lnType, lnPri, lnName)
  }
}
