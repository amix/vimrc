'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Position, Range, SelectionRange } from 'vscode-languageserver-protocol'
import languages from '../languages'
import { HandlerDelegate } from '../types'
import { equals } from '../util/object'
import { positionInRange } from '../util/position'
import window from '../window'
import workspace from '../workspace'

export default class SelectionRangeHandler {
  private selectionRange: SelectionRange = null
  constructor(private nvim: Neovim, private handler: HandlerDelegate) {
  }

  public async getSelectionRanges(): Promise<SelectionRange[] | null> {
    let { doc, position } = await this.handler.getCurrentState()
    this.handler.checkProvier('selectionRange', doc.textDocument)
    await doc.synchronize()
    let selectionRanges: SelectionRange[] = await this.handler.withRequestToken('selection ranges', token => {
      return languages.getSelectionRanges(doc.textDocument, [position], token)
    })
    return selectionRanges
  }

  public async selectRange(visualmode: string, forward: boolean): Promise<void> {
    let { nvim } = this
    let { doc } = await this.handler.getCurrentState()
    this.handler.checkProvier('selectionRange', doc.textDocument)
    let positions: Position[] = []
    if (!forward && (!this.selectionRange || !visualmode)) return
    if (visualmode) {
      let range = await window.getSelectedRange(visualmode)
      positions.push(range.start, range.end)
    } else {
      let position = await window.getCursorPosition()
      positions.push(position)
    }
    if (!forward) {
      let curr = Range.create(positions[0], positions[1])
      let { selectionRange } = this
      while (selectionRange && selectionRange.parent) {
        if (equals(selectionRange.parent.range, curr)) {
          break
        }
        selectionRange = selectionRange.parent
      }
      if (selectionRange && selectionRange.parent) {
        await window.selectRange(selectionRange.range)
      }
      return
    }
    await doc.synchronize()
    let selectionRanges: SelectionRange[] = await this.handler.withRequestToken('selection ranges', token => {
      return languages.getSelectionRanges(doc.textDocument, positions, token)
    })
    if (!selectionRanges || selectionRanges.length == 0) return
    let mode = await nvim.eval('mode()')
    if (mode != 'n') await nvim.eval(`feedkeys("\\<Esc>", 'in')`)
    let selectionRange: SelectionRange
    if (selectionRanges.length == 1) {
      selectionRange = selectionRanges[0]
    } else {
      let end = positions[1] || positions[0]
      let r = Range.create(positions[0], end)
      selectionRange = selectionRanges[0]
      while (selectionRange) {
        if (equals(r, selectionRange.range)) {
          selectionRange = selectionRange.parent
          continue
        }
        if (
          positionInRange(positions[0], selectionRange.range) == 0 &&
          positionInRange(end, selectionRange.range) == 0) {
          break
        }
        selectionRange = selectionRange.parent
      }
    }
    if (!selectionRange) return
    this.selectionRange = selectionRanges[0]
    await window.selectRange(selectionRange.range)
  }
}
