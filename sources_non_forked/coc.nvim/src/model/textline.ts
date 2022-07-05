'use strict'
import { Range } from 'vscode-languageserver-protocol'

/**
 * Represents a line of text, such as a line of source code.
 *
 * TextLine objects are __immutable__. When a {@link TextDocument document} changes,
 * previously retrieved lines will not represent the latest state.
 */
export class TextLine {

  private readonly _line: number
  private readonly _text: string
  private readonly _isLastLine: boolean

  constructor(line: number, text: string, isLastLine: boolean) {
    this._line = line
    this._text = text
    this._isLastLine = isLastLine
  }

  /**
   * The zero-based line number.
   */
  public get lineNumber(): number {
    return this._line
  }

  /**
   * The text of this line without the line separator characters.
   */
  public get text(): string {
    return this._text
  }

  /**
   * The range this line covers without the line separator characters.
   */
  public get range(): Range {
    return Range.create(this._line, 0, this._line, this._text.length)
  }

  /**
   * The range this line covers with the line separator characters.
   */
  public get rangeIncludingLineBreak(): Range {
    return this._isLastLine ? this.range : Range.create(this._line, 0, this._line + 1, 0)
  }

  /**
   * The offset of the first character which is not a whitespace character as defined
   * by `/\s/`. **Note** that if a line is all whitespace the length of the line is returned.
   */
  public get firstNonWhitespaceCharacterIndex(): number {
    // TODO@api, rename to 'leadingWhitespaceLength'
    return /^(\s*)/.exec(this._text)![1].length
  }

  /**
   * Whether this line is whitespace only, shorthand
   * for {@link TextLine.firstNonWhitespaceCharacterIndex} === {@link TextLine.text TextLine.text.length}.
   */
  public get isEmptyOrWhitespace(): boolean {
    return this.firstNonWhitespaceCharacterIndex === this._text.length
  }
}
