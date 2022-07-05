'use strict'
import { MarkupContent, TextEdit, Position, Location, Command } from 'vscode-languageserver-protocol'
import * as Is from './util/is'

/**
 * Inlay hint kinds.
 *
 * @since 3.17.0
 * @proposed
 */
export namespace InlayHintKind {

  /**
   * An inlay hint that for a type annotation.
   */
  export const Type = 1

  /**
   * An inlay hint that is for a parameter.
   */
  export const Parameter = 2

  export function is(value: number): value is InlayHintKind {
    return value === 1 || value === 2
  }
}

// eslint-disable-next-line no-redeclare
export type InlayHintKind = 1 | 2

/**
 * An inlay hint label part allows for interactive and composite labels
 * of inlay hints.
 *
 * @since 3.17.0
 * @proposed
 */
export interface InlayHintLabelPart {

  /**
   * The value of this label part.
   */
  value: string

  /**
   * The tooltip text when you hover over this label part. Depending on
   * the client capability `inlayHint.resolveSupport` clients might resolve
   * this property late using the resolve request.
   */
  tooltip?: string | MarkupContent

  /**
   * An optional source code location that represents this
   * label part.
   *
   * The editor will use this location for the hover and for code navigation
   * features: This part will become a clickable link that resolves to the
   * definition of the symbol at the given location (not necessarily the
   * location itself), it shows the hover that shows at the given location,
   * and it shows a context menu with further code navigation commands.
   *
   * Depending on the client capability `inlayHint.resolveSupport` clients
   * might resolve this property late using the resolve request.
   */
  location?: Location

  /**
   * An optional command for this label part.
   *
   * Depending on the client capability `inlayHint.resolveSupport` clients
   * might resolve this property late using the resolve request.
   */
  command?: Command
}

// eslint-disable-next-line no-redeclare
export namespace InlayHintLabelPart {

  export function create(value: string): InlayHintLabelPart {
    return { value }
  }

  export function is(value: any): value is InlayHintLabelPart {
    const candidate: InlayHintLabelPart = value
    return Is.objectLiteral(candidate)
      && (candidate.tooltip === undefined || Is.string(candidate.tooltip) || MarkupContent.is(candidate.tooltip))
      && (candidate.location === undefined || Location.is(candidate.location))
      && (candidate.command === undefined || Command.is(candidate.command))
  }
}

/**
 * Inlay hint information.
 *
 * @since 3.17.0
 * @proposed
 */
export interface InlayHint {

  /**
   * The position of this hint.
   */
  position: Position

  /**
   * The label of this hint. A human readable string or an array of
   * InlayHintLabelPart label parts.
   *
   * *Note* that neither the string nor the label part can be empty.
   */
  label: string | InlayHintLabelPart[]

  /**
   * The kind of this hint. Can be omitted in which case the client
   * should fall back to a reasonable default.
   */
  kind?: InlayHintKind

  /**
   * Optional text edits that are performed when accepting this inlay hint.
   *
   * *Note* that edits are expected to change the document so that the inlay
   * hint (or its nearest variant) is now part of the document and the inlay
   * hint itself is now obsolete.
   */
  textEdits?: TextEdit[]

  /**
   * The tooltip text when you hover over this item.
   */
  tooltip?: string | MarkupContent

  /**
   * Render padding before the hint.
   *
   * Note: Padding should use the editor's background color, not the
   * background color of the hint itself. That means padding can be used
   * to visually align/separate an inlay hint.
   */
  paddingLeft?: boolean

  /**
   * Render padding after the hint.
   *
   * Note: Padding should use the editor's background color, not the
   * background color of the hint itself. That means padding can be used
   * to visually align/separate an inlay hint.
   */
  paddingRight?: boolean

  /**
   * A data entry field that is preserved on a inlay hint between
   * a `textDocument/inlayHint` and a `inlayHint/resolve` request.
   */
  data?: any
}

// eslint-disable-next-line no-redeclare
export namespace InlayHint {

  export function create(position: Position, label: string | InlayHintLabelPart[], kind?: InlayHintKind): InlayHint {
    const result: InlayHint = { position, label }
    if (kind !== undefined) {
      result.kind = kind
    }
    return result
  }

  export function is(value: any): value is InlayHint {
    const candidate: InlayHint = value
    return Is.objectLiteral(candidate) && Position.is(candidate.position)
      && (Is.string(candidate.label) || Is.typedArray(candidate.label, InlayHintLabelPart.is))
      && (candidate.kind === undefined || InlayHintKind.is(candidate.kind))
      && (candidate.textEdits === undefined) || Is.typedArray(candidate.textEdits, TextEdit.is)
      && (candidate.tooltip === undefined || Is.string(candidate.tooltip) || MarkupContent.is(candidate.tooltip))
      && (candidate.paddingLeft === undefined || Is.boolean(candidate.paddingLeft))
      && (candidate.paddingRight === undefined || Is.boolean(candidate.paddingRight))
  }
}
