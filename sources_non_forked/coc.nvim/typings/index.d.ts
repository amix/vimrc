/******************************************************************
MIT License http://www.opensource.org/licenses/mit-license.php
Author Qiming Zhao <chemzqm@gmail> (https://github.com/chemzqm)
*******************************************************************/

/// <reference types="node" />
import cp from 'child_process'

declare module 'coc.nvim' {
  // Language server protocol interfaces {{
  export interface Thenable<T> {
    then<TResult>(onfulfilled?: (value: T) => TResult | Thenable<TResult>, onrejected?: (reason: any) => TResult | Thenable<TResult>): Thenable<TResult>
    // eslint-disable-next-line @typescript-eslint/unified-signatures
    then<TResult>(onfulfilled?: (value: T) => TResult | Thenable<TResult>, onrejected?: (reason: any) => void): Thenable<TResult>
  }

  export interface Disposable {
    /**
     * Dispose this object.
     */
    dispose(): void
  }

  export namespace Disposable {
    function create(func: () => void): Disposable
  }
  /**
   * The declaration of a symbol representation as one or many [locations](#Location).
   */
  export type Declaration = Location | Location[]
  /**
   * Information about where a symbol is declared.
   *
   * Provides additional metadata over normal [location](#Location) declarations, including the range of
   * the declaring symbol.
   *
   * Servers should prefer returning `DeclarationLink` over `Declaration` if supported
   * by the client.
   */
  export type DeclarationLink = LocationLink

  export type ProgressToken = number | string

  export interface WorkDoneProgressBegin {
    kind: 'begin'
    /**
     * Mandatory title of the progress operation. Used to briefly inform about
     * the kind of operation being performed.
     *
     * Examples: "Indexing" or "Linking dependencies".
     */
    title: string
    /**
     * Controls if a cancel button should show to allow the user to cancel the
     * long running operation. Clients that don't support cancellation are allowed
     * to ignore the setting.
     */
    cancellable?: boolean
    /**
     * Optional, more detailed associated progress message. Contains
     * complementary information to the `title`.
     *
     * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
     * If unset, the previous progress message (if any) is still valid.
     */
    message?: string
    /**
     * Optional progress percentage to display (value 100 is considered 100%).
     * If not provided infinite progress is assumed and clients are allowed
     * to ignore the `percentage` value in subsequent in report notifications.
     *
     * The value should be steadily rising. Clients are free to ignore values
     * that are not following this rule.
     */
    percentage?: number
  }

  export interface WorkDoneProgressReport {
    kind: 'report'
    /**
     * Controls enablement state of a cancel button. This property is only valid if a cancel
     * button got requested in the `WorkDoneProgressStart` payload.
     *
     * Clients that don't support cancellation or don't support control the button's
     * enablement state are allowed to ignore the setting.
     */
    cancellable?: boolean
    /**
     * Optional, more detailed associated progress message. Contains
     * complementary information to the `title`.
     *
     * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
     * If unset, the previous progress message (if any) is still valid.
     */
    message?: string
    /**
     * Optional progress percentage to display (value 100 is considered 100%).
     * If not provided infinite progress is assumed and clients are allowed
     * to ignore the `percentage` value in subsequent in report notifications.
     *
     * The value should be steadily rising. Clients are free to ignore values
     * that are not following this rule.
     */
    percentage?: number
  }

  /**
   * The file event type
   */
  export namespace FileChangeType {
    /**
     * The file got created.
     */
    const Created = 1
    /**
     * The file got changed.
     */
    const Changed = 2
    /**
     * The file got deleted.
     */
    const Deleted = 3
  }

  export type FileChangeType = 1 | 2 | 3

  /**
   * An event describing a file change.
   */
  export interface FileEvent {
    /**
     * The file's uri.
     */
    uri: string
    /**
     * The change type.
     */
    type: FileChangeType
  }

  export interface WorkDoneProgressEnd {
    kind: 'end'
    /**
     * Optional, a final message indicating to for example indicate the outcome
     * of the operation.
     */
    message?: string
  }

  /**
   * A literal to identify a text document in the client.
   */
  export interface TextDocumentIdentifier {
    /**
     * The text document's uri.
     */
    uri: string
  }

  /**
   * A parameter literal used in requests to pass a text document and a position inside that
   * document.
   */
  export interface TextDocumentPositionParams {
    /**
     * The text document.
     */
    textDocument: TextDocumentIdentifier
    /**
     * The position inside the text document.
     */
    position: Position
  }

  export interface WorkspaceFolder {
    /**
     * The associated URI for this workspace folder.
     */
    uri: string
    /**
     * The name of the workspace folder. Used to refer to this
     * workspace folder in the user interface.
     */
    name: string
  }

  /**
   * An event describing a change to a text document.
   */
  export interface TextDocumentContentChange {
    /**
     * The range of the document that changed.
     */
    range: Range
    /**
     * The new text for the provided range.
     */
    text: string
  }

  /**
   * The workspace folder change event.
   */
  export interface WorkspaceFoldersChangeEvent {
    /**
     * The array of added workspace folders
     */
    added: WorkspaceFolder[]
    /**
     * The array of the removed workspace folders
     */
    removed: WorkspaceFolder[]
  }

  /**
   * An event that is fired when a [document](#LinesTextDocument) will be saved.
   *
   * To make modifications to the document before it is being saved, call the
   * [`waitUntil`](#TextDocumentWillSaveEvent.waitUntil)-function with a thenable
   * that resolves to an array of [text edits](#TextEdit).
   */
  export interface TextDocumentWillSaveEvent {

    /**
     * The document that will be saved.
     */
    document: LinesTextDocument

    /**
     * The reason why save was triggered.
     */
    reason: 1 | 2 | 3
  }

  /**
   * A document filter denotes a document by different properties like
   * the [language](#LinesTextDocument.languageId), the [scheme](#Uri.scheme) of
   * its resource, or a glob-pattern that is applied to the [path](#LinesTextDocument.fileName).
   *
   * Glob patterns can have the following syntax:
   * - `*` to match one or more characters in a path segment
   * - `?` to match on one character in a path segment
   * - `**` to match any number of path segments, including none
   * - `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
   * - `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
   * - `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
   *
   * @sample A language filter that applies to typescript files on disk: `{ language: 'typescript', scheme: 'file' }`
   * @sample A language filter that applies to all package.json paths: `{ language: 'json', pattern: '**package.json' }`
   */
  export type DocumentFilter = {
    /** A language id, like `typescript`. */
    language: string
    /** A Uri [scheme](#Uri.scheme), like `file` or `untitled`. */
    scheme?: string
    /** A glob pattern, like `*.{ts,js}`. */
    pattern?: string
  } | {
    /** A language id, like `typescript`. */
    language?: string
    /** A Uri [scheme](#Uri.scheme), like `file` or `untitled`. */
    scheme: string
    /** A glob pattern, like `*.{ts,js}`. */
    pattern?: string
  } | {
    /** A language id, like `typescript`. */
    language?: string
    /** A Uri [scheme](#Uri.scheme), like `file` or `untitled`. */
    scheme?: string
    /** A glob pattern, like `*.{ts,js}`. */
    pattern: string
  }
  /**
   * A document selector is the combination of one or many document filters.
   *
   * @sample `let sel:DocumentSelector = [{ language: 'typescript' }, { language: 'json', pattern: '**∕tsconfig.json' }]`;
   */
  export type DocumentSelector = (string | DocumentFilter)[]
  /**
   * A selection range represents a part of a selection hierarchy. A selection range
   * may have a parent selection range that contains it.
   */
  export interface SelectionRange {
    /**
     * The [range](#Range) of this selection range.
     */
    range: Range
    /**
     * The parent selection range containing this range. Therefore `parent.range` must contain `this.range`.
     */
    parent?: SelectionRange
  }

  /**
   * MarkedString can be used to render human readable text. It is either a markdown string
   * or a code-block that provides a language and a code snippet. The language identifier
   * is semantically equal to the optional language identifier in fenced code blocks in GitHub
   * issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
   *
   * The pair of a language and a value is an equivalent to markdown:
   * ```${language}
   * ${value}
   * ```
   *
   * Note that markdown strings will be sanitized - that means html will be escaped.
   * @deprecated use MarkupContent instead.
   */
  export type MarkedString = string | {
    language: string
    value: string
  }
  /**
   * The result of a hover request.
   */
  export interface Hover {
    /**
     * The hover's content
     */
    contents: MarkupContent | MarkedString | MarkedString[]
    /**
     * An optional range
     */
    range?: Range
  }

  /**
   * The definition of a symbol represented as one or many [locations](#Location).
   * For most programming languages there is only one location at which a symbol is
   * defined.
   *
   * Servers should prefer returning `DefinitionLink` over `Definition` if supported
   * by the client.
   */
  export type Definition = Location | Location[]

  /**
   * Information about where a symbol is defined.
   *
   * Provides additional metadata over normal [location](#Location) definitions, including the range of
   * the defining symbol
   */
  export type DefinitionLink = LocationLink

  /**
   * How a signature help was triggered.
   */
  export namespace SignatureHelpTriggerKind {
    /**
    * Signature help was invoked manually by the user or by a command.
    */
    const Invoked: 1
    /**
    * Signature help was triggered by a trigger character.
    */
    const TriggerCharacter: 2
    /**
    * Signature help was triggered by the cursor moving or by the document content changing.
    */
    const ContentChange: 3
  }

  export type SignatureHelpTriggerKind = 1 | 2 | 3

  /**
   * Represents the signature of something callable. A signature
   * can have a label, like a function-name, a doc-comment, and
   * a set of parameters.
   */
  export interface SignatureInformation {
    /**
     * The label of this signature. Will be shown in
     * the UI.
     */
    label: string
    /**
     * The human-readable doc-comment of this signature. Will be shown
     * in the UI but can be omitted.
     */
    documentation?: string | MarkupContent
    /**
     * The parameters of this signature.
     */
    parameters?: ParameterInformation[]
  }

  /**
   * Represents a parameter of a callable-signature. A parameter can
   * have a label and a doc-comment.
   */
  export interface ParameterInformation {
    /**
     * The label of this parameter information.
     *
     * Either a string or an inclusive start and exclusive end offsets within its containing
     * signature label. (see SignatureInformation.label). The offsets are based on a UTF-16
     * string representation as `Position` and `Range` does.
     *
     * *Note*: a label of type string should be a substring of its containing signature label.
     * Its intended use case is to highlight the parameter label part in the `SignatureInformation.label`.
     */
    label: string | [number, number]
    /**
     * The human-readable doc-comment of this signature. Will be shown
     * in the UI but can be omitted.
     */
    documentation?: string | MarkupContent
  }

  /**
   * Signature help represents the signature of something
   * callable. There can be multiple signature but only one
   * active and only one active parameter.
   */
  export interface SignatureHelp {
    /**
     * One or more signatures.
     */
    signatures: SignatureInformation[]
    /**
     * The active signature. Set to `null` if no
     * signatures exist.
     */
    activeSignature: number | null
    /**
     * The active parameter of the active signature. Set to `null`
     * if the active signature has no parameters.
     */
    activeParameter: number | null
  }
  /**
   * Additional information about the context in which a signature help request was triggered.
   *
   * @since 3.15.0
   */
  export interface SignatureHelpContext {
    /**
     * Action that caused signature help to be triggered.
     */
    triggerKind: SignatureHelpTriggerKind
    /**
     * Character that caused signature help to be triggered.
     *
     * This is undefined when `triggerKind !== SignatureHelpTriggerKind.TriggerCharacter`
     */
    triggerCharacter?: string
    /**
     * `true` if signature help was already showing when it was triggered.
     *
     * Retriggers occur when the signature help is already active and can be caused by actions such as
     * typing a trigger character, a cursor move, or document content changes.
     */
    isRetrigger: boolean
    /**
     * The currently active `SignatureHelp`.
     *
     * The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field updated based on
     * the user navigating through available signatures.
     */
    activeSignatureHelp?: SignatureHelp
  }

  /**
   * Represents a folding range.
   */
  export interface FoldingRange {
    /**
     * The zero-based line number from where the folded range starts.
     */
    startLine: number
    /**
     * The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line.
     */
    startCharacter?: number
    /**
     * The zero-based line number where the folded range ends.
     */
    endLine: number
    /**
     * The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line.
     */
    endCharacter?: number
    /**
     * Describes the kind of the folding range such as `comment' or 'region'. The kind
     * is used to categorize folding ranges and used by commands like 'Fold all comments'. See
     * [FoldingRangeKind](#FoldingRangeKind) for an enumeration of standardized kinds.
     */
    kind?: string
  }

  /**
   * A symbol kind.
   */
  export namespace SymbolKind {
    const File: 1
    const Module: 2
    const Namespace: 3
    const Package: 4
    const Class: 5
    const Method: 6
    const Property: 7
    const Field: 8
    const Constructor: 9
    const Enum: 10
    const Interface: 11
    const Function: 12
    const Variable: 13
    const Constant: 14
    const String: 15
    const Number: 16
    const Boolean: 17
    const Array: 18
    const Object: 19
    const Key: 20
    const Null: 21
    const EnumMember: 22
    const Struct: 23
    const Event: 24
    const Operator: 25
    const TypeParameter: 26
  }

  export type SymbolKind = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26

  /**
   * Represents information about programming constructs like variables, classes,
   * interfaces etc.
   */
  export interface SymbolInformation {
    /**
     * The name of this symbol.
     */
    name: string
    /**
     * The kind of this symbol.
     */
    kind: SymbolKind
    /**
     * Indicates if this symbol is deprecated.
     */
    deprecated?: boolean
    /**
     * The location of this symbol. The location's range is used by a tool
     * to reveal the location in the editor. If the symbol is selected in the
     * tool the range's start information is used to position the cursor. So
     * the range usually spans more than the actual symbol's name and does
     * normally include thinks like visibility modifiers.
     *
     * The range doesn't have to denote a node range in the sense of a abstract
     * syntax tree. It can therefore not be used to re-construct a hierarchy of
     * the symbols.
     */
    location: Location
    /**
     * The name of the symbol containing this symbol. This information is for
     * user interface purposes (e.g. to render a qualifier in the user interface
     * if necessary). It can't be used to re-infer a hierarchy for the document
     * symbols.
     */
    containerName?: string
  }

  /**
   * Represents programming constructs like variables, classes, interfaces etc.
   * that appear in a document. Document symbols can be hierarchical and they
   * have two ranges: one that encloses its definition and one that points to
   * its most interesting range, e.g. the range of an identifier.
   */
  export interface DocumentSymbol {
    /**
     * The name of this symbol. Will be displayed in the user interface and therefore must not be
     * an empty string or a string only consisting of white spaces.
     */
    name: string
    /**
     * More detail for this symbol, e.g the signature of a function.
     */
    detail?: string
    /**
     * The kind of this symbol.
     */
    kind: SymbolKind
    /**
     * Indicates if this symbol is deprecated.
     */
    deprecated?: boolean
    /**
     * The range enclosing this symbol not including leading/trailing whitespace but everything else
     * like comments. This information is typically used to determine if the the clients cursor is
     * inside the symbol to reveal in the symbol in the UI.
     */
    range: Range
    /**
     * The range that should be selected and revealed when this symbol is being picked, e.g the name of a function.
     * Must be contained by the the `range`.
     */
    selectionRange: Range
    /**
     * Children of this symbol, e.g. properties of a class.
     */
    children?: DocumentSymbol[]
  }

  export interface FormattingOptions {
    /**
     * If indentation is based on spaces (`insertSpaces` = true), the number of spaces that make an indent.
     */
    tabSize: number
    /**
     * Is indentation based on spaces?
     */
    insertSpaces: boolean
    /**
     * Trim trailing whitespaces on a line.
     *
     * @since 3.15.0
     */
    trimTrailingWhitespace?: boolean
    /**
     * Insert a newline character at the end of the file if one does not exist.
     *
     * @since 3.15.0
     */
    insertFinalNewline?: boolean
    /**
     * Trim all newlines after the final newline at the end of the file.
     *
     * @since 3.15.0
     */
    trimFinalNewlines?: boolean
  }

  /**
   * Contains additional diagnostic information about the context in which
   * a [code action](#CodeActionProvider.provideCodeActions) is run.
   */
  export interface CodeActionContext {
    /**
     * An array of diagnostics known on the client side overlapping the range provided to the
     * `textDocument/codeAction` request. They are provided so that the server knows which
     * errors are currently presented to the user for the given range. There is no guarantee
     * that these accurately reflect the error state of the resource. The primary parameter
     * to compute code actions is the provided range.
     */
    diagnostics: Diagnostic[]
    /**
     * Requested kind of actions to return.
     *
     * Actions not of this kind are filtered out by the client before being shown. So servers
     * can omit computing them.
     */
    only?: string[]
  }


  /**
   * A document highlight kind.
   */
  export namespace DocumentHighlightKind {
    /**
     * A textual occurrence.
     */
    const Text: 1
    /**
     * Read-access of a symbol, like reading a variable.
     */
    const Read: 2
    /**
     * Write-access of a symbol, like writing to a variable.
     */
    const Write: 3
  }

  export type DocumentHighlightKind = 1 | 2 | 3
  /**
   * A document highlight is a range inside a text document which deserves
   * special attention. Usually a document highlight is visualized by changing
   * the background color of its range.
   */
  export interface DocumentHighlight {
    /**
     * The range this highlight applies to.
     */
    range: Range
    /**
     * The highlight kind, default is [text](#DocumentHighlightKind.Text).
     */
    kind?: DocumentHighlightKind
  }

  /**
   * A document link is a range in a text document that links to an internal or external resource, like another
   * text document or a web site.
   */
  export interface DocumentLink {
    /**
     * The range this link applies to.
     */
    range: Range
    /**
     * The uri this link points to.
     */
    target?: string
    /**
     * The tooltip text when you hover over this link.
     *
     * If a tooltip is provided, is will be displayed in a string that includes instructions on how to
     * trigger the link, such as `{0} (ctrl + click)`. The specific instructions vary depending on OS,
     * user settings, and localization.
     *
     * @since 3.15.0
     */
    tooltip?: string
    /**
     * A data entry field that is preserved on a document link between a
     * DocumentLinkRequest and a DocumentLinkResolveRequest.
     */
    data?: any
  }

  /**
   * Represents a color in RGBA space.
   */
  export interface Color {
    /**
     * The red component of this color in the range [0-1].
     */
    readonly red: number
    /**
     * The green component of this color in the range [0-1].
     */
    readonly green: number
    /**
     * The blue component of this color in the range [0-1].
     */
    readonly blue: number
    /**
     * The alpha component of this color in the range [0-1].
     */
    readonly alpha: number
  }

  /**
   * Represents a color range from a document.
   */
  export interface ColorInformation {
    /**
     * The range in the document where this color appears.
     */
    range: Range
    /**
     * The actual color value for this color range.
     */
    color: Color
  }

  export interface ColorPresentation {
    /**
     * The label of this color presentation. It will be shown on the color
     * picker header. By default this is also the text that is inserted when selecting
     * this color presentation.
     */
    label: string
    /**
     * An [edit](#TextEdit) which is applied to a document when selecting
     * this presentation for the color.  When `falsy` the [label](#ColorPresentation.label)
     * is used.
     */
    textEdit?: TextEdit
    /**
     * An optional array of additional [text edits](#TextEdit) that are applied when
     * selecting this color presentation. Edits must not overlap with the main [edit](#ColorPresentation.textEdit) nor with themselves.
     */
    additionalTextEdits?: TextEdit[]
  }

  /**
   * A code lens represents a [command](#Command) that should be shown along with
   * source text, like the number of references, a way to run tests, etc.
   *
   * A code lens is _unresolved_ when no command is associated to it. For performance
   * reasons the creation of a code lens and resolving should be done to two stages.
   */
  export interface CodeLens {
    /**
     * The range in which this code lens is valid. Should only span a single line.
     */
    range: Range
    /**
     * The command this code lens represents.
     */
    command?: Command
    /**
     * An data entry field that is preserved on a code lens item between
     * a [CodeLensRequest](#CodeLensRequest) and a [CodeLensResolveRequest]
     * (#CodeLensResolveRequest)
     */
    data?: any
  }

  /**
   * Represents the connection of two locations. Provides additional metadata over normal [locations](#Location),
   * including an origin range.
   */
  export interface LocationLink {
    /**
     * Span of the origin of this link.
     *
     * Used as the underlined span for mouse definition hover. Defaults to the word range at
     * the definition position.
     */
    originSelectionRange?: Range
    /**
     * The target resource identifier of this link.
     */
    targetUri: string
    /**
     * The full target range of this link. If the target for example is a symbol then target range is the
     * range enclosing this symbol not including leading/trailing whitespace but everything else
     * like comments. This information is typically used to highlight the range in the editor.
     */
    targetRange: Range
    /**
     * The range that should be selected and revealed when this link is being followed, e.g the name of a function.
     * Must be contained by the the `targetRange`. See also `DocumentSymbol#range`
     */
    targetSelectionRange: Range
  }

  /**
   * The LocationLink namespace provides helper functions to work with
   * [LocationLink](#LocationLink) literals.
   */
  export namespace LocationLink {
    /**
     * Creates a LocationLink literal.
     * @param targetUri The definition's uri.
     * @param targetRange The full range of the definition.
     * @param targetSelectionRange The span of the symbol definition at the target.
     * @param originSelectionRange The span of the symbol being defined in the originating source file.
    */
    function create(targetUri: string, targetRange: Range, targetSelectionRange: Range, originSelectionRange?: Range): LocationLink
    /**
     * Checks whether the given literal conforms to the [LocationLink](#LocationLink) interface.
     */
    function is(value: any): value is LocationLink
  }

  export type MarkupKind = 'plaintext' | 'markdown'

  /**
   * Describes the content type that a client supports in various
   * result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
   *
   * Please note that `MarkupKinds` must not start with a `$`. This kinds
   * are reserved for internal usage.
   */
  export namespace MarkupKind {
    /**
     * Plain text is supported as a content format
     */
    const PlainText: 'plaintext'
    /**
     * Markdown is supported as a content format
     */
    const Markdown: 'markdown'
  }
  /**
   * A `MarkupContent` literal represents a string value which content is interpreted base on its
   * kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
   *
   * If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
   * See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
   *
   * Here is an example how such a string can be constructed using JavaScript / TypeScript:
   * ```ts
   * let markdown: MarkdownContent = {
   *  kind: MarkupKind.Markdown,
   *	value: [
   *		'# Header',
   *		'Some text',
   *		'```typescript',
   *		'someCode();',
   *		'```'
   *	].join('\n')
   * };
   * ```
   *
   * *Please Note* that clients might sanitize the return markdown. A client could decide to
   * remove HTML from the markdown to avoid script execution.
   */
  export interface MarkupContent {
    /**
     * The type of the Markup
     */
    kind: MarkupKind
    /**
     * The content itself
     */
    value: string
  }

  /**
   * The kind of a completion entry.
   */
  export namespace CompletionItemKind {
    const Text: 1
    const Method: 2
    const Function: 3
    const Constructor: 4
    const Field: 5
    const Variable: 6
    const Class: 7
    const Interface: 8
    const Module: 9
    const Property: 10
    const Unit: 11
    const Value: 12
    const Enum: 13
    const Keyword: 14
    const Snippet: 15
    const Color: 16
    const File: 17
    const Reference: 18
    const Folder: 19
    const EnumMember: 20
    const Constant: 21
    const Struct: 22
    const Event: 23
    const Operator: 24
    const TypeParameter: 25
  }

  export type CompletionItemKind = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25

  /**
   * Defines whether the insert text in a completion item should be interpreted as
   * plain text or a snippet.
   */
  export namespace InsertTextFormat {
    /**
     * The primary text to be inserted is treated as a plain string.
     */
    const PlainText: 1
    /**
     * The primary text to be inserted is treated as a snippet.
     *
     * A snippet can define tab stops and placeholders with `$1`, `$2`
     * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
     * the end of the snippet. Placeholders with equal identifiers are linked,
     * that is typing in one will update others too.
     *
     * See also: https://github.com/microsoft/vscode/blob/main/src/vs/editor/contrib/snippet/snippet.md
     */
    const Snippet: 2
  }
  export type InsertTextFormat = 1 | 2

  /**
   * A completion item represents a text snippet that is
   * proposed to complete text that is being typed.
   */
  export interface CompletionItem {
    /**
     * The label of this completion item. By default
     * also the text that is inserted when selecting
     * this completion.
     */
    label: string
    /**
     * The kind of this completion item. Based of the kind
     * an icon is chosen by the editor.
     */
    kind?: CompletionItemKind
    /**
     * Tags for this completion item.
     *
     * @since 3.15.0
     */
    tags?: number[]
    /**
     * A human-readable string with additional information
     * about this item, like type or symbol information.
     */
    detail?: string
    /**
     * A human-readable string that represents a doc-comment.
     */
    documentation?: string | MarkupContent
    /**
     * Indicates if this item is deprecated.
     * @deprecated Use `tags` instead.
     */
    deprecated?: boolean
    /**
     * Select this item when showing.
     *
     * *Note* that only one completion item can be selected and that the
     * tool / client decides which item that is. The rule is that the *first*
     * item of those that match best is selected.
     */
    preselect?: boolean
    /**
     * A string that should be used when comparing this item
     * with other items. When `falsy` the [label](#CompletionItem.label)
     * is used.
     */
    sortText?: string
    /**
     * A string that should be used when filtering a set of
     * completion items. When `falsy` the [label](#CompletionItem.label)
     * is used.
     */
    filterText?: string
    /**
     * A string that should be inserted into a document when selecting
     * this completion. When `falsy` the [label](#CompletionItem.label)
     * is used.
     *
     * The `insertText` is subject to interpretation by the client side.
     * Some tools might not take the string literally. For example
     * VS Code when code complete is requested in this example `con<cursor position>`
     * and a completion item with an `insertText` of `console` is provided it
     * will only insert `sole`. Therefore it is recommended to use `textEdit` instead
     * since it avoids additional client side interpretation.
     */
    insertText?: string
    /**
     * The format of the insert text. The format applies to both the `insertText` property
     * and the `newText` property of a provided `textEdit`. If omitted defaults to
     * `InsertTextFormat.PlainText`.
     */
    insertTextFormat?: InsertTextFormat
    /**
     * An [edit](#TextEdit) which is applied to a document when selecting
     * this completion. When an edit is provided the value of
     * [insertText](#CompletionItem.insertText) is ignored.
     *
     * *Note:* The text edit's range must be a [single line] and it must contain the position
     * at which completion has been requested.
     */
    textEdit?: TextEdit
    /**
     * An optional array of additional [text edits](#TextEdit) that are applied when
     * selecting this completion. Edits must not overlap (including the same insert position)
     * with the main [edit](#CompletionItem.textEdit) nor with themselves.
     *
     * Additional text edits should be used to change text unrelated to the current cursor position
     * (for example adding an import statement at the top of the file if the completion item will
     * insert an unqualified type).
     */
    additionalTextEdits?: TextEdit[]
    /**
     * An optional set of characters that when pressed while this completion is active will accept it first and
     * then type that character. *Note* that all commit characters should have `length=1` and that superfluous
     * characters will be ignored.
     */
    commitCharacters?: string[]
    /**
     * An optional [command](#Command) that is executed *after* inserting this completion. *Note* that
     * additional modifications to the current document should be described with the
     * [additionalTextEdits](#CompletionItem.additionalTextEdits)-property.
     */
    command?: Command
    /**
     * An data entry field that is preserved on a completion item between
     * a [CompletionRequest](#CompletionRequest) and a [CompletionResolveRequest]
     * (#CompletionResolveRequest)
     */
    data?: any
  }

  /**
   * Represents a collection of [completion items](#CompletionItem) to be presented
   * in the editor.
   */
  export interface CompletionList {
    /**
     * This list it not complete. Further typing results in recomputing this list.
     */
    isIncomplete: boolean
    /**
     * The completion items.
     */
    items: CompletionItem[]
  }

  /**
 * How a completion was triggered
 */
  export namespace CompletionTriggerKind {
    /**
     * Completion was triggered by typing an identifier (24x7 code
     * complete), manual invocation (e.g Ctrl+Space) or via API.
     */
    const Invoked: 1
    /**
     * Completion was triggered by a trigger character specified by
     * the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
     */
    const TriggerCharacter: 2
    /**
     * Completion was re-triggered as current completion list is incomplete
     */
    const TriggerForIncompleteCompletions: 3
  }

  export type CompletionTriggerKind = 1 | 2 | 3

  /**
   * Contains additional information about the context in which a completion request is triggered.
   */
  export interface CompletionContext {
    /**
     * How the completion was triggered.
     */
    triggerKind: CompletionTriggerKind,
    /**
     * The trigger character (a single character) that has trigger code complete.
     * Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`
     */
    triggerCharacter?: string

    option?: CompleteOption
  }

  /**
   * Represents a reference to a command. Provides a title which
   * will be used to represent a command in the UI and, optionally,
   * an array of arguments which will be passed to the command handler
   * function when invoked.
   */
  export interface Command {
    /**
     * Title of the command, like `save`.
     */
    title: string
    /**
     * The identifier of the actual command handler.
     */
    command: string
    /**
     * Arguments that the command handler should be
     * invoked with.
     */
    arguments?: any[]
  }

  export interface TextDocumentEdit {
    /**
     * The text document to change.
     */
    textDocument: {
      uri: string
      version: number | null
    }
    /**
     * The edits to be applied.
     */
    edits: TextEdit[]
  }

  /**
   * A workspace edit represents changes to many resources managed in the workspace. The edit
   * should either provide `changes` or `documentChanges`. If documentChanges are present
   * they are preferred over `changes` if the client can handle versioned document edits.
   */
  export interface WorkspaceEdit {
    /**
     * Holds changes to existing resources.
     */
    changes?: {
      [uri: string]: TextEdit[]
    }
    /**
     * Depending on the client capability `workspace.workspaceEdit.resourceOperations` document changes
     * are either an array of `TextDocumentEdit`s to express changes to n different text documents
     * where each text document edit addresses a specific version of a text document. Or it can contain
     * above `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.
     *
     * Whether a client supports versioned document edits is expressed via
     * `workspace.workspaceEdit.documentChanges` client capability.
     *
     * If a client neither supports `documentChanges` nor `workspace.workspaceEdit.resourceOperations` then
     * only plain `TextEdit`s using the `changes` property are supported.
     */
    documentChanges?: (TextDocumentEdit | CreateFile | RenameFile | DeleteFile)[]
  }

  interface ResourceOperation {
    kind: string
  }

  /**
   * Delete file options
   */
  export interface DeleteFileOptions {
    /**
     * Delete the content recursively if a folder is denoted.
     */
    recursive?: boolean
    /**
     * Ignore the operation if the file doesn't exist.
     */
    ignoreIfNotExists?: boolean
  }
  /**
   * Delete file operation
   */
  export interface DeleteFile extends ResourceOperation {
    /**
     * A delete
     */
    kind: 'delete'
    /**
     * The file to delete.
     */
    uri: string
    /**
     * Delete options.
     */
    options?: DeleteFileOptions
  }

  /**
   * Options to create a file.
   */
  export interface CreateFileOptions {
    /**
     * Overwrite existing file. Overwrite wins over `ignoreIfExists`
     */
    overwrite?: boolean
    /**
     * Ignore if exists.
     */
    ignoreIfExists?: boolean
  }
  /**
   * Create file operation.
   */
  export interface CreateFile extends ResourceOperation {
    /**
     * A create
     */
    kind: 'create'
    /**
     * The resource to create.
     */
    uri: string
    /**
     * Additional options
     */
    options?: CreateFileOptions
  }

  /**
   * Rename file options
   */
  export interface RenameFileOptions {
    /**
     * Overwrite target if existing. Overwrite wins over `ignoreIfExists`
     */
    overwrite?: boolean
    /**
     * Ignores if target exists.
     */
    ignoreIfExists?: boolean
  }
  /**
   * Rename file operation
   */
  export interface RenameFile extends ResourceOperation {
    /**
     * A rename
     */
    kind: 'rename'
    /**
     * The old (existing) location.
     */
    oldUri: string
    /**
     * The new location.
     */
    newUri: string
    /**
     * Rename options.
     */
    options?: RenameFileOptions
  }
  /**
   * Represents a related message and source code location for a diagnostic. This should be
   * used to point to code locations that cause or related to a diagnostics, e.g when duplicating
   * a symbol in a scope.
   */
  export interface DiagnosticRelatedInformation {
    /**
     * The location of this related diagnostic information.
     */
    location: Location
    /**
     * The message of this related diagnostic information.
     */
    message: string
  }

  /**
   * The diagnostic's severity.
   */
  export namespace DiagnosticSeverity {
    /**
     * Reports an error.
     */
    const Error: 1
    /**
     * Reports a warning.
     */
    const Warning: 2
    /**
     * Reports an information.
     */
    const Information: 3
    /**
     * Reports a hint.
     */
    const Hint: 4
  }

  export type DiagnosticSeverity = 1 | 2 | 3 | 4

  /**
   * The diagnostic tags.
   *
   * @since 3.15.0
   */
  export namespace DiagnosticTag {
    /**
     * Unused or unnecessary code.
     *
     * Clients are allowed to render diagnostics with this tag faded out instead of having
     * an error squiggle.
     */
    const Unnecessary: 1
    /**
     * Deprecated or obsolete code.
     *
     * Clients are allowed to rendered diagnostics with this tag strike through.
     */
    const Deprecated: 2
  }

  export type DiagnosticTag = 1 | 2

  /**
   * Represents a diagnostic, such as a compiler error or warning. Diagnostic objects
   * are only valid in the scope of a resource.
   */
  export interface Diagnostic {
    /**
     * The range at which the message applies
     */
    range: Range
    /**
     * The diagnostic's severity. Can be omitted. If omitted it is up to the
     * client to interpret diagnostics as error, warning, info or hint.
     */
    severity?: DiagnosticSeverity
    /**
     * The diagnostic's code, which usually appear in the user interface.
     */
    code?: number | string
    /**
     * A human-readable string describing the source of this
     * diagnostic, e.g. 'typescript' or 'super lint'. It usually
     * appears in the user interface.
     */
    source?: string
    /**
     * The diagnostic's message. It usually appears in the user interface
     */
    message: string
    /**
     * Additional metadata about the diagnostic.
     */
    tags?: DiagnosticTag[]
    /**
     * An array of related diagnostic information, e.g. when symbol-names within
     * a scope collide all definitions can be marked via this property.
     */
    relatedInformation?: DiagnosticRelatedInformation[]
  }

  /**
   * The Diagnostic namespace provides helper functions to work with
   * [Diagnostic](#Diagnostic) literals.
   */
  export namespace Diagnostic {
    /**
     * Creates a new Diagnostic literal.
     */
    function create(range: Range, message: string, severity?: DiagnosticSeverity, code?: number | string, source?: string, relatedInformation?: DiagnosticRelatedInformation[]): Diagnostic
    /**
     * Checks whether the given literal conforms to the [Diagnostic](#Diagnostic) interface.
     */
    function is(value: any): value is Diagnostic
  }

  /**
   * A code action represents a change that can be performed in code, e.g. to fix a problem or
   * to refactor code.
   *
   * A CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed.
   */
  export interface CodeAction {
    /**
     * A short, human-readable, title for this code action.
     */
    title: string
    /**
     * The kind of the code action.
     *
     * Used to filter code actions.
     */
    kind?: CodeActionKind
    /**
     * The diagnostics that this code action resolves.
     */
    diagnostics?: Diagnostic[]
    /**
     * Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted
     * by keybindings.
     *
     * A quick fix should be marked preferred if it properly addresses the underlying error.
     * A refactoring should be marked preferred if it is the most reasonable choice of actions to take.
     *
     * @since 3.15.0
     */
    isPreferred?: boolean
    /**
     * The workspace edit this code action performs.
     */
    edit?: WorkspaceEdit
    /**
     * A command this code action executes. If a code action
     * provides a edit and a command, first the edit is
     * executed and then the command.
     */
    command?: Command
    /**
     * Id of client that provide codeAction.
     */
    clientId?: string
  }

  /**
   * The kind of a code action.
   *
   * Kinds are a hierarchical list of identifiers separated by `.`, e.g. `"refactor.extract.function"`.
   *
   * The set of kinds is open and client needs to announce the kinds it supports to the server during
   * initialization.
   */
  export type CodeActionKind = string
  /**
   * A set of predefined code action kinds
   */
  export namespace CodeActionKind {
    /**
     * Empty kind.
     */
    const Empty: CodeActionKind
    /**
     * Base kind for quickfix actions: 'quickfix'
     */
    const QuickFix: CodeActionKind
    /**
     * Base kind for refactoring actions: 'refactor'
     */
    const Refactor: CodeActionKind
    /**
     * Base kind for refactoring extraction actions: 'refactor.extract'
     *
     * Example extract actions:
     *
     * - Extract method
     * - Extract function
     * - Extract variable
     * - Extract interface from class
     * - ...
     */
    const RefactorExtract: CodeActionKind
    /**
     * Base kind for refactoring inline actions: 'refactor.inline'
     *
     * Example inline actions:
     *
     * - Inline function
     * - Inline variable
     * - Inline constant
     * - ...
     */
    const RefactorInline: CodeActionKind
    /**
     * Base kind for refactoring rewrite actions: 'refactor.rewrite'
     *
     * Example rewrite actions:
     *
     * - Convert JavaScript function to class
     * - Add or remove parameter
     * - Encapsulate field
     * - Make method static
     * - Move method to base class
     * - ...
     */
    const RefactorRewrite: CodeActionKind
    /**
     * Base kind for source actions: `source`
     *
     * Source code actions apply to the entire file.
     */
    const Source: CodeActionKind
    /**
     * Base kind for an organize imports source action: `source.organizeImports`
     */
    const SourceOrganizeImports: CodeActionKind
    /**
     * Base kind for auto-fix source actions: `source.fixAll`.
     *
     * Fix all actions automatically fix errors that have a clear fix that do not require user input.
     * They should not suppress errors or perform unsafe fixes such as generating new types or classes.
     *
     * @since 3.15.0
     */
    const SourceFixAll: CodeActionKind
  }

  /**
   * Position in a text document expressed as zero-based line and character offset.
   * The offsets are based on a UTF-16 string representation. So a string of the form
   * `a𐐀b` the character offset of the character `a` is 0, the character offset of `𐐀`
   * is 1 and the character offset of b is 3 since `𐐀` is represented using two code
   * units in UTF-16.
   *
   * Positions are line end character agnostic. So you can not specify a position that
   * denotes `\r|\n` or `\n|` where `|` represents the character offset.
   */
  export interface Position {
    /**
     * Line position in a document (zero-based).
     * If a line number is greater than the number of lines in a document, it defaults back to the number of lines in the document.
     * If a line number is negative, it defaults to 0.
     */
    line: number
    /**
     * Character offset on a line in a document (zero-based). Assuming that the line is
     * represented as a string, the `character` value represents the gap between the
     * `character` and `character + 1`.
     *
     * If the character value is greater than the line length it defaults back to the
     * line length.
     * If a line number is negative, it defaults to 0.
     */
    character: number
  }

  /**
   * The Position namespace provides helper functions to work with
   * [Position](#Position) literals.
   */
  export namespace Position {
    /**
     * Creates a new Position literal from the given line and character.
     * @param line The position's line.
     * @param character The position's character.
     */
    function create(line: number, character: number): Position
    /**
     * Checks whether the given liternal conforms to the [Position](#Position) interface.
     */
    function is(value: any): value is Position
  }

  /**
   * Represents a typed event.
   *
   * A function that represents an event to which you subscribe by calling it with
   * a listener function as argument.
   *
   * @example
   * item.onDidChange(function(event) { console.log("Event happened: " + event); });
   */
  export interface Event<T> {

    /**
     * A function that represents an event to which you subscribe by calling it with
     * a listener function as argument.
     *
     * @param listener The listener function will be called when the event happens.
     * @param thisArgs The `this`-argument which will be used when calling the event listener.
     * @param disposables An array to which a [disposable](#Disposable) will be added.
     * @return A disposable which unsubscribes the event listener.
     */
    (listener: (e: T) => any, thisArgs?: any, disposables?: Disposable[]): Disposable
  }

  export namespace Event {
    const None: Event<any>
  }

  export interface EmitterOptions {
    onFirstListenerAdd?: Function
    onLastListenerRemove?: Function
  }

  export class Emitter<T> {
    constructor(_options?: EmitterOptions | undefined)
    /**
     * For the public to allow to subscribe
     * to events from this Emitter
     */
    get event(): Event<T>
    /**
     * To be kept private to fire an event to
     * subscribers
     */
    fire(event: T): any
    dispose(): void
  }

  /**
   * Represents a location inside a resource, such as a line
   * inside a text file.
   */
  export interface Location {
    uri: string
    range: Range
  }

  /**
   * The Location namespace provides helper functions to work with
   * [Location](#Location) literals.
   */
  export namespace Location {
    /**
     * Creates a Location literal.
     * @param uri The location's uri.
     * @param range The location's range.
     */
    function create(uri: string, range: Range): Location
    /**
     * Checks whether the given literal conforms to the [Location](#Location) interface.
     */
    function is(value: any): value is Location
  }

  /**
   * A range in a text document expressed as (zero-based) start and end positions.
   *
   * If you want to specify a range that contains a line including the line ending
   * character(s) then use an end position denoting the start of the next line.
   * For example:
   * ```ts
   * {
   *     start: { line: 5, character: 23 }
   *     end : { line 6, character : 0 }
   * }
   * ```
   */
  export interface Range {
    /**
     * The range's start position
     */
    start: Position
    /**
     * The range's end position.
     */
    end: Position
  }

  /**
   * The Range namespace provides helper functions to work with
   * [Range](#Range) literals.
   */
  export namespace Range {
    /**
     * Create a new Range liternal.
     * @param start The range's start position.
     * @param end The range's end position.
     */
    function create(start: Position, end: Position): Range
    /**
     * Create a new Range liternal.
     * @param startLine The start line number.
     * @param startCharacter The start character.
     * @param endLine The end line number.
     * @param endCharacter The end character.
     */
    function create(startLine: number, startCharacter: number, endLine: number, endCharacter: number): Range
    /**
     * Checks whether the given literal conforms to the [Range](#Range) interface.
     */
    function is(value: any): value is Range
  }

  /**
   * A text edit applicable to a text document.
   */
  export interface TextEdit {
    /**
     * The range of the text document to be manipulated. To insert
     * text into a document create a range where start === end.
     */
    range: Range
    /**
     * The string to be inserted. For delete operations use an
     * empty string.
     */
    newText: string
  }

  /**
   * The TextEdit namespace provides helper function to create replace,
   * insert and delete edits more easily.
   */
  export namespace TextEdit {
    /**
     * Creates a replace text edit.
     * @param range The range of text to be replaced.
     * @param newText The new text.
     */
    function replace(range: Range, newText: string): TextEdit
    /**
     * Creates a insert text edit.
     * @param position The position to insert the text at.
     * @param newText The text to be inserted.
     */
    function insert(position: Position, newText: string): TextEdit
    /**
     * Creates a delete text edit.
     * @param range The range of text to be deleted.
     */
    function del(range: Range): TextEdit
    function is(value: any): value is TextEdit
  }

  /**
   * Defines a CancellationToken. This interface is not
   * intended to be implemented. A CancellationToken must
   * be created via a CancellationTokenSource.
   */
  export interface CancellationToken {
    /**
     * Is `true` when the token has been cancelled, `false` otherwise.
     */
    readonly isCancellationRequested: boolean
    /**
     * An [event](#Event) which fires upon cancellation.
     */
    readonly onCancellationRequested: Event<any>
  }

  export namespace CancellationToken {
    const None: CancellationToken
    const Cancelled: CancellationToken
    function is(value: any): value is CancellationToken
  }

  export class CancellationTokenSource {
    get token(): CancellationToken
    cancel(): void
    dispose(): void
  }

  /**
   * Represents a line of text, such as a line of source code.
   *
   * TextLine objects are __immutable__. When a {@link LinesTextDocument document} changes,
   * previously retrieved lines will not represent the latest state.
   */
  export interface TextLine {
    /**
     * The zero-based line number.
     */
    readonly lineNumber: number

    /**
     * The text of this line without the line separator characters.
     */
    readonly text: string

    /**
     * The range this line covers without the line separator characters.
     */
    readonly range: Range

    /**
     * The range this line covers with the line separator characters.
     */
    readonly rangeIncludingLineBreak: Range

    /**
     * The offset of the first character which is not a whitespace character as defined
     * by `/\s/`. **Note** that if a line is all whitespace the length of the line is returned.
     */
    readonly firstNonWhitespaceCharacterIndex: number

    /**
     * Whether this line is whitespace only, shorthand
     * for {@link TextLine.firstNonWhitespaceCharacterIndex} === {@link TextLine.text TextLine.text.length}.
     */
    readonly isEmptyOrWhitespace: boolean
  }

  /**
   * A simple text document. Not to be implemented. The document keeps the content
   * as string.
   */
  export interface TextDocument {
    /**
     * The associated URI for this document. Most documents have the __file__-scheme, indicating that they
     * represent files on disk. However, some documents may have other schemes indicating that they are not
     * available on disk.
     *
     * @readonly
     */
    readonly uri: string
    /**
     * The identifier of the language associated with this document.
     *
     * @readonly
     */
    readonly languageId: string
    /**
     * The version number of this document (it will increase after each
     * change, including undo/redo).
     *
     * @readonly
     */
    readonly version: number
    /**
     * Get the text of this document. A substring can be retrieved by
     * providing a range.
     *
     * @param range (optional) An range within the document to return.
     * If no range is passed, the full content is returned.
     * Invalid range positions are adjusted as described in [Position.line](#Position.line)
     * and [Position.character](#Position.character).
     * If the start range position is greater than the end range position,
     * then the effect of getText is as if the two positions were swapped.

     * @return The text of this document or a substring of the text if a
     *         range is provided.
     */
    getText(range?: Range): string
    /**
     * Converts a zero-based offset to a position.
     *
     * @param offset A zero-based offset.
     * @return A valid [position](#Position).
     */
    positionAt(offset: number): Position
    /**
     * Converts the position to a zero-based offset.
     * Invalid positions are adjusted as described in [Position.line](#Position.line)
     * and [Position.character](#Position.character).
     *
     * @param position A position.
     * @return A valid zero-based offset.
     */
    offsetAt(position: Position): number
    /**
     * The number of lines in this document.
     *
     * @readonly
     */
    readonly lineCount: number
  }

  export interface LinesTextDocument extends TextDocument {
    /**
     * Total length of TextDocument.
     */
    readonly length: number
    /**
     * End position of TextDocument.
     */
    readonly end: Position
    /**
     * 'eol' option of related buffer. When enabled additional `\n` will be
     * added to the end of document content
     */
    readonly rol: boolean
    /**
     * Lines of TextDocument.
     */
    readonly lines: ReadonlyArray<string>
    /**
     * Returns a text line denoted by the line number. Note
     * that the returned object is *not* live and changes to the
     * document are not reflected.
     *
     * @param line or position
     * @return A {@link TextLine line}.
     */
    lineAt(lineOrPos: number | Position): TextLine
  }

  /**
   * @since 3.16.0
   */
  export interface SemanticTokensLegend {
    /**
     * The token types a server uses.
     */
    tokenTypes: string[]
    /**
     * The token modifiers a server uses.
     */
    tokenModifiers: string[]
  }

  /**
  * @since 3.16.0
  */
  export interface SemanticTokens {
    /**
    * An optional result id. If provided and clients support delta updating
    * the client will include the result id in the next semantic token request.
    * A server can then instead of computing all semantic tokens again simply
    * send a delta.
    */
    resultId?: string
    /**
    * The actual tokens.
    */
    data: number[]
  }

  /**
   * @since 3.16.0
   */
  export interface SemanticTokensEdit {
    /**
     * The start offset of the edit.
     */
    start: number
    /**
     * The count of elements to remove.
     */
    deleteCount: number
    /**
     * The elements to insert.
     */
    data?: number[]
  }

  /**
   * @since 3.16.0
   */
  export interface SemanticTokensDelta {
    readonly resultId?: string
    /**
     * The semantic token edits to transform a previous result into a new result.
     */
    edits: SemanticTokensEdit[]
  }

  /**
  * The result of a linked editing range request.
  *
  * @since 3.16.0
  */
  export interface LinkedEditingRanges {
    /**
    * A list of ranges that can be edited together. The ranges must have
    * identical length and contain identical text content. The ranges cannot overlap.
    */
    ranges: Range[]
    /**
    * An optional word pattern (regular expression) that describes valid contents for
    * the given ranges. If no pattern is provided, the client configuration's word
    * pattern will be used.
    */
    wordPattern?: string
  }

  /**
   * Represents programming constructs like functions or constructors in the context
   * of call hierarchy.
   *
   * @since 3.16.0
   */
  export interface CallHierarchyItem {
    /**
     * The name of this item.
     */
    name: string
    /**
     * The kind of this item.
     */
    kind: SymbolKind
    /**
     * Tags for this item.
     */
    tags?: number[]
    /**
     * More detail for this item, e.g. the signature of a function.
     */
    detail?: string
    /**
     * The resource identifier of this item.
     */
    uri: string
    /**
     * The range enclosing this symbol not including leading/trailing whitespace but everything else, e.g. comments and code.
     */
    range: Range
    /**
     * The range that should be selected and revealed when this symbol is being picked, e.g. the name of a function.
     * Must be contained by the [`range`](#CallHierarchyItem.range).
     */
    selectionRange: Range
    /**
     * A data entry field that is preserved between a call hierarchy prepare and
     * incoming calls or outgoing calls requests.
     */
    data?: unknown
  }

  /**
  * Represents an incoming call, e.g. a caller of a method or constructor.
  *
  * @since 3.16.0
  */
  export interface CallHierarchyIncomingCall {
    /**
    * The item that makes the call.
    */
    from: CallHierarchyItem
    /**
    * The ranges at which the calls appear. This is relative to the caller
    * denoted by [`this.from`](#CallHierarchyIncomingCall.from).
    */
    fromRanges: Range[]
  }
  /**
  * Represents an outgoing call, e.g. calling a getter from a method or a method from a constructor etc.
  *
  * @since 3.16.0
  */
  export interface CallHierarchyOutgoingCall {
    /**
    * The item that is called.
    */
    to: CallHierarchyItem
    /**
    * The range at which this item is called. This is the range relative to the caller, e.g the item
    * passed to [`provideCallHierarchyOutgoingCalls`](#CallHierarchyItemProvider.provideCallHierarchyOutgoingCalls)
    * and not [`this.to`](#CallHierarchyOutgoingCall.to).
    */
    fromRanges: Range[]
  }

  /**
   * Moniker uniqueness level to define scope of the moniker.
   *
   * @since 3.16.0
   */
  export namespace UniquenessLevel {
    /**
     * The moniker is only unique inside a document
     */
    export const document = 'document'

    /**
     * The moniker is unique inside a project for which a dump got created
     */
    export const project = 'project'

    /**
     * The moniker is unique inside the group to which a project belongs
     */
    export const group = 'group'

    /**
     * The moniker is unique inside the moniker scheme.
     */
    export const scheme = 'scheme'

    /**
     * The moniker is globally unique
     */
    export const global = 'global'
  }

  export type UniquenessLevel = 'document' | 'project' | 'group' | 'scheme' | 'global'

  /**
   * The moniker kind.
   *
   * @since 3.16.0
   */
  export enum MonikerKind {
    /**
     * The moniker represent a symbol that is imported into a project
     */
    import = 'import',

    /**
     * The moniker represents a symbol that is exported from a project
     */
    export = 'export',

    /**
     * The moniker represents a symbol that is local to a project (e.g. a local
     * variable of a function, a class not visible outside the project, ...)
     */
    local = 'local'
  }

  /**
   * Moniker definition to match LSIF 0.5 moniker definition.
   *
   * @since 3.16.0
   */
  export interface Moniker {
    /**
     * The scheme of the moniker. For example tsc or .Net
     */
    scheme: string

    /**
     * The identifier of the moniker. The value is opaque in LSIF however
     * schema owners are allowed to define the structure if they want.
     */
    identifier: string

    /**
     * The scope in which the moniker is unique
     */
    unique: UniquenessLevel

    /**
     * The moniker kind if known.
     */
    kind?: MonikerKind
  }

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
  // }}

  // nvim interfaces {{
  type VimValue =
    | number
    | boolean
    | string
    | number[]
    | { [key: string]: any }

  // see `:h nvim_set_client_info()` for details.
  export interface VimClientInfo {
    name: string
    version: {
      major?: number
      minor?: number
      patch?: number
      prerelease?: string
      commit?: string
    }
    type: 'remote' | 'embedder' | 'host'
    methods?: {
      [index: string]: any
    }
    attributes?: {
      [index: string]: any
    }
  }

  export interface UiAttachOptions {
    rgb?: boolean
    ext_popupmenu?: boolean
    ext_tabline?: boolean
    ext_wildmenu?: boolean
    ext_cmdline?: boolean
    ext_linegrid?: boolean
    ext_hlstate?: boolean
  }

  export interface ChanInfo {
    id: number
    stream: 'stdio' | 'stderr' | 'socket' | 'job'
    mode: 'bytes' | 'terminal' | 'rpc'
    pty?: number
    buffer?: number
    client?: VimClientInfo
  }

  /**
   * Returned by nvim_get_commands api.
   */
  export interface VimCommandDescription {
    name: string
    bang: boolean
    bar: boolean
    register: boolean
    definition: string
    count?: number | null
    script_id: number
    complete?: string
    nargs?: string
    range?: string
    complete_arg?: string
  }

  export interface NvimFloatOptions {
    standalone?: boolean
    focusable?: boolean
    relative?: 'editor' | 'cursor' | 'win'
    anchor?: 'NW' | 'NE' | 'SW' | 'SE'
    height: number
    width: number
    row: number
    col: number
  }

  export interface ExtmarkOptions {
    id?: number
    // 0-based inclusive.
    end_line?: number
    // 0-based exclusive.
    end_col?: number
    //  name of the highlight group used to highlight this mark.
    hl_group?: string
    hl_mode?: 'replace' | 'combine' | 'blend'
    hl_eol?: boolean
    // A list of [text, highlight] tuples
    virt_text?: [string, string | string[]][]
    virt_text_pos?: 'eol' | 'overlay' | 'right_align'
    virt_text_win_col?: number
    virt_text_hide?: boolean
    virt_lines?: [string, string | string[]][][]
    virt_lines_above?: boolean
    virt_lines_leftcol?: boolean
    right_gravity?: boolean
    end_right_gravity?: boolean
    priority?: number
  }

  export interface ExtmarkDetails {
    end_col: number
    end_row: number
    priority: number
    hl_group?: string
    virt_text?: [string, string][]
    virt_lines?: [string, string | string][][]
  }

  export interface NvimProc {
    ppid: number
    name: string
    pid: number
  }

  export interface SignPlaceOption {
    id?: number
    group?: string
    name: string
    lnum: number
    priority?: number
  }

  export interface SignUnplaceOption {
    group?: string
    id?: number
  }

  export interface SignPlacedOption {
    /**
     * Use '*' for all group, default to '' as unnamed group.
     */
    group?: string
    id?: number
    lnum?: number
  }

  export interface SignItem {
    group: string
    id: number
    lnum: number
    name: string
    priority: number
  }

  export interface HighlightItem {
    hlGroup: string
    /**
    * 0 based
    */
    lnum: number
    /**
    * 0 based
    */
    colStart: number
    /**
    * 0 based
    */
    colEnd: number
  }

  export interface ExtendedHighlightItem extends HighlightItem {
    combine?: boolean
    start_incl?: boolean
    end_incl?: boolean
  }

  export interface HighlightOption {
    /**
     * 0 based start line, default to 0.
     */
    start?: number
    /**
     * 0 based end line, default to 0.
     */
    end?: number
    /**
     * Default to 0 on vim8, 4096 on neovim
     */
    priority?: number
    /**
     * Buffer changedtick to match.
     */
    changedtick?: number
  }

  export interface BufferKeymapOption {
    nowait?: boolean
    silent?: boolean
    script?: boolean
    expr?: boolean
    unique?: boolean
  }

  export interface BufferHighlight {
    /**
     * Name of the highlight group to use
     */
    hlGroup?: string
    /**
     * Namespace to use or -1 for ungrouped highlight
     */
    srcId?: number
    /**
     * Line to highlight (zero-indexed)
     */
    line?: number
    /**
     * Start of (byte-indexed) column range to highlight
     */
    colStart?: number
    /**
     * End of (byte-indexed) column range to highlight, or -1 to highlight to end of line
     */
    colEnd?: number
  }

  export interface BufferClearHighlight {
    srcId?: number
    lineStart?: number
    lineEnd?: number
  }

  interface BaseApi<T> {
    /**
     * unique identify number
     */
    id: number

    /**
     * Check if same by compare id.
     */
    equals(other: T): boolean

    /**
     * Request to vim, name need to be nvim_ prefixed and supported by vim.
     *
     * @param {string} name - nvim function name
     * @param {VimValue[]} args
     * @returns {Promise<VimValue>}
     */
    request(name: string, args?: VimValue[]): Promise<any>

    /**
     * Send notification to vim, name need to be nvim_ prefixed and supported
     * by vim
     */
    notify(name: string, args?: VimValue[]): void

    /**
     * Retrieves scoped variable, returns null when value doesn't exist.
     */
    getVar(name: string): Promise<VimValue | null>

    /**
     * Set scoped variable by request.
     *
     * @param {string} name
     * @param {VimValue} value
     * @returns {Promise<void>}
     */
    setVar(name: string, value: VimValue): Promise<void>

    /**
     * Set scoped variable by notification.
     */
    setVar(name: string, value: VimValue, isNotify: true): void

    /**
     * Delete scoped variable by notification.
     */
    deleteVar(name: string): void

    /**
     * Retrieves a scoped option, doesn't exist for tabpage.
     */
    getOption(name: string): Promise<VimValue>

    /**
     * Set scoped option by request, doesn't exist for tabpage.
     */
    setOption(name: string, value: VimValue): Promise<void>

    /**
     * Set scoped  variable by notification, doesn't exist for tabpage.
     */
    setOption(name: string, value: VimValue, isNotify: true): void
  }

  export interface Neovim extends BaseApi<Neovim> {

    /**
     * Echo error message to vim and log error stack.
     */
    echoError(msg: unknown): void

    /**
     * Check if `nvim_` function exists.
     */
    hasFunction(name: string): boolean

    /**
     * Get channelid used by coc.nvim.
     */
    channelId: Promise<number>

    /**
     * Create buffer instance by id.
     */
    createBuffer(id: number): Buffer

    /**
     * Create window instance by id.
     */
    createWindow(id: number): Window

    /**
     * Create tabpage instance by id.
     */
    createTabpage(id: number): Tabpage

    /**
     * Stop send subsequent notifications.
     * This method **must** be paired with `nvim.resumeNotification` in a sync manner.
     */
    pauseNotification(): void

    /**
     * Send paused notifications by nvim_call_atomic request
     *
     * @param {boolean} redrawVim Redraw vim on vim8 to update the screen
     *
     * **Note**: avoid call async function between pauseNotification and
     * resumeNotification.
     */
    resumeNotification(redrawVim?: boolean): Promise<[VimValue[], [string, number, string] | null]>

    /**
     * Send paused notifications by nvim_call_atomic notification
     *
     * @param {boolean} redrawVim Redraw vim to update the screen
     * @param {true} notify
     * @returns {void}
     */
    resumeNotification(redrawVim: boolean, notify: true): void

    /**
     * Send redraw command to vim, does nothing on neovim since it's not necessary on most cases.
     */
    redrawVim(): void

    /**
     * Get list of current buffers.
     */
    buffers: Promise<Buffer[]>

    /**
     * Get current buffer.
     */
    buffer: Promise<Buffer>

    /**
     * Set current buffer
     */
    setBuffer(buffer: Buffer): Promise<void>

    /**
     * Get list of current tabpages.
     */
    tabpages: Promise<Tabpage[]>

    /**
     * Get current tabpage.
     */
    tabpage: Promise<Tabpage>

    /**
     * Set current tabpage
     */
    setTabpage(tabpage: Tabpage): Promise<void>

    /**
     * Get list of current windows.
     */
    windows: Promise<Window[]>

    /**
     * Get current window.
     */
    window: Promise<Window>

    /**
     * Set current window.
     */
    setWindow(window: Window): Promise<void>

    /**
     * Get information of all channels,
     * **Note:** works on neovim only.
     */
    chans: Promise<ChanInfo[]>

    /**
     * Get information of channel by id,
     * **Note:** works on neovim only.
     */
    getChanInfo(id: number): Promise<ChanInfo>

    /**
     * Creates a new namespace, or gets an existing one.
     * `:h nvim_create_namespace()`
     */
    createNamespace(name?: string): Promise<number>

    /**
     * Gets existing, non-anonymous namespaces.
     *
     * @return dict that maps from names to namespace ids.
     */
    namespaces: Promise<{ [name: string]: number }>

    /**
     * Gets a map of global (non-buffer-local) Ex commands.
     *
     * @return Map of maps describing commands.
     */
    getCommands(opt?: { builtin: boolean }): Promise<{ [name: string]: VimCommandDescription }>

    /**
     * Get list of all runtime paths
     */
    runtimePaths: Promise<string[]>

    /**
     * Set global working directory.
     * **Note:** works on neovim only.
     */
    setDirectory(dir: string): Promise<void>

    /**
     * Get current line.
     */
    line: Promise<string>

    /**
     * Creates a new, empty, unnamed buffer.
     *
     * **Note:** works on neovim only.
     */
    createNewBuffer(listed?: boolean, scratch?: boolean): Promise<Buffer>

    /**
     * Create float window of neovim.
     *
     * **Note:** works on neovim only, use high level api provided by window
     * module is recommended.
     */
    openFloatWindow(buffer: Buffer, enter: boolean, options: NvimFloatOptions): Promise<Window>

    /**
     * Set current line.
     */
    setLine(line: string): Promise<void>

    /**
     * Gets a list of global (non-buffer-local) |mapping| definitions.
     * `:h nvim_get_keymap`
     *
     * **Note:** works on neovim only.
     */
    getKeymap(mode: string): Promise<object[]>

    /**
     * Gets the current mode. |mode()| "blocking" is true if Nvim is waiting for input.
     *
     * **Note:** blocking would always be false when used with vim.
     */
    mode: Promise<{ mode: string; blocking: boolean }>

    /**
     * Returns a map of color names and RGB values.
     *
     * **Note:** works on neovim only.
     */
    colorMap(): Promise<{ [name: string]: number }>

    /**
     * Returns the 24-bit RGB value of a |nvim_get_color_map()| color name or
     * "#rrggbb" hexadecimal string.
     *
     * **Note:** works on neovim only.
     */
    getColorByName(name: string): Promise<number>

    /**
     * Gets a highlight definition by id. |hlID()|
     *
     * **Note:** works on neovim only.
     */
    getHighlight(nameOrId: string | number, isRgb?: boolean): Promise<object>

    /**
     * Get a highlight by name, return rgb by default.
     *
     * **Note:** works on neovim only.
     */
    getHighlightByName(name: string, isRgb?: boolean): Promise<object>

    /**
     * Get a highlight by id, return rgb by default.
     *
     * **Note:** works on neovim only.
     */
    getHighlightById(id: number, isRgb?: boolean): Promise<object>

    /**
     * Delete current line in buffer.
     */
    deleteCurrentLine(): Promise<void>

    /**
     * Evaluates a VimL expression (:help expression). Dictionaries
     * and Lists are recursively expanded. On VimL error: Returns a
     * generic error; v:errmsg is not updated.
     *
     */
    eval(expr: string): Promise<VimValue>

    /**
     * Executes lua, it's possible neovim client does not support this
     *
     * **Note:** works on neovim only.
     */
    lua(code: string, args?: VimValue[]): Promise<object>

    /**
     * Calls a VimL |Dictionary-function| with the given arguments.
     */
    callDictFunction(dict: object | string, fname: string, args: VimValue | VimValue[]): Promise<object>

    /**
     * Call a vim function.
     *
     * @param {string} fname - function name
     * @param {VimValue | VimValue[]} args
     * @returns {Promise<any>}
     */
    call(fname: string, args?: VimValue | VimValue[]): Promise<any>

    /**
     * Call a vim function by notification.
     */
    call(fname: string, args: VimValue | VimValue[], isNotify: true): void

    /**
     * Call a vim function with timer of timeout 0.
     *
     * @param {string} fname - function name
     * @param {VimValue | VimValue[]} args
     * @returns {Promise<any>}
     */
    callTimer(fname: string, args?: VimValue | VimValue[]): Promise<void>

    /**
     * Call a vim function with timer of timeout 0 by notification.
     */
    callTimer(fname: string, args: VimValue | VimValue[], isNotify: true): void

    /**
     * Call async vim function that accept callback as argument
     * by using notifications.
     */
    callAsync(fname: string, args?: VimValue | VimValue[]): Promise<unknown>

    /**
     * Calls many API methods atomically.
     */
    callAtomic(calls: [string, VimValue[]][]): Promise<[any[], any[] | null]>

    /**
     * Executes an ex-command by request.
     */
    command(arg: string): Promise<void>

    /**
     * Executes an ex-command by notification.
     */
    command(arg: string, isNotify: true): void

    /**
     * Runs a command and returns output.
     *
     * @deprecated Use exec() instead.
     */
    commandOutput(arg: string): Promise<string>

    /**
     * Executes Vimscript (multiline block of Ex-commands), like
     * anonymous |:source|
     */
    exec(src: string, output?: boolean): Promise<string>

    /**
     * Gets a v: variable.
     */
    getVvar(name: string): Promise<VimValue>

    /**
     * `:h nvim_feedkeys`
     */
    feedKeys(keys: string, mode: string, escapeCsi: boolean): Promise<void>

    /**
     * Queues raw user-input. Unlike |nvim_feedkeys()|, this uses a
     * low-level input buffer and the call is non-blocking (input is
     * processed asynchronously by the eventloop).
     *
     * On execution error: does not fail, but updates v:errmsg.
     *
     * **Note:** works on neovim only.
     */
    input(keys: string): Promise<number>

    /**
     * Parse a VimL Expression.
     */
    parseExpression(expr: string, flags: string, highlight: boolean): Promise<object>

    /**
     * Get process info, neovim only.
     *
     * **Note:** works on neovim only.
     */
    getProc(pid: number): Promise<NvimProc>

    /**
     * Gets the immediate children of process `pid`.
     *
     * **Note:** works on neovim only.
     */
    getProcChildren(pid: number): Promise<NvimProc[]>

    /**
     * Replaces terminal codes and |keycodes| (<CR>, <Esc>, ...)
     * in a string with the internal representation.
     *
     * **Note:** works on neovim only.
     */
    replaceTermcodes(str: string, fromPart: boolean, doIt: boolean, special: boolean): Promise<string>

    /**
     * Gets width(display cells) of string.
     */
    strWidth(str: string): Promise<number>

    /**
     * Gets a list of dictionaries representing attached UIs.
     *
     * **Note:** works on neovim only.
     */
    uis: Promise<any[]>

    /**
     * Subscribe to nvim event broadcasts.
     *
     * **Note:** works on neovim only.
     */
    subscribe(event: string): Promise<void>

    /**
     * Unsubscribe to nvim event broadcasts
     *
     * **Note:** works on neovim only.
     */
    unsubscribe(event: string): Promise<void>

    /**
     * Activates UI events on the channel.
     *
     * **Note:** works on neovim only.
     */
    uiAttach(width: number, height: number, options: UiAttachOptions): Promise<void>

    /**
     * `:h nvim_ui_try_resize`
     *
     * **Note:** works on neovim only.
     */
    uiTryResize(width: number, height: number): Promise<void>

    /**
     * Deactivates UI events on the channel.
     *
     * **Note:** works on neovim only.
     */
    uiDetach(): Promise<void>

    /**
     * Quit vim.
     */
    quit(): Promise<void>
  }

  export interface Buffer extends BaseApi<Buffer> {
    id: number

    /** Total number of lines in buffer */
    length: Promise<number>

    /**
     * Get lines of buffer.
     */
    lines: Promise<string[]>

    /**
     * Get changedtick of buffer.
     */
    changedtick: Promise<number>

    /**
     * Add buffer keymap by notification.
     */
    setKeymap(mode: string, lhs: string, rhs: string, opts?: BufferKeymapOption): void

    /**
     * Removes an ext mark by notification.
     *
     * @public
     * @param {number} ns_id - Namespace id
     * @param {number} id - Extmark id
     */
    deleteExtMark(ns_id: number, id: number): void

    /**
     * Gets the position (0-indexed) of an extmark.
     *
     * @param {number} ns_id - Namespace id
     * @param {number} id - Extmark id
     * @param {Object} opts - Optional parameters.
     * @returns {Promise<[] | [number, number] | [number, number, ExtmarkDetails]>}
     */
    getExtMarkById(ns_id: number, id: number, opts?: {
      details?: boolean
    }): Promise<[] | [number, number] | [number, number, ExtmarkDetails]>

    /**
     * Gets extmarks in "traversal order" from a |charwise| region defined by
     * buffer positions (inclusive, 0-indexed |api-indexing|).
     *
     * Region can be given as (row,col) tuples, or valid extmark ids (whose
     * positions define the bounds). 0 and -1 are understood as (0,0) and (-1,-1)
     * respectively, thus the following are equivalent:
     *
     *     nvim_buf_get_extmarks(0, my_ns, 0, -1, {})
     *     nvim_buf_get_extmarks(0, my_ns, [0,0], [-1,-1], {})
     *
     * @param {number} ns_id - Namespace id
     * @param {[number, number] | number} start
     * @param {[number, number] | number} end
     * @param {Object} opts
     * @returns {Promise<[number, number, number, ExtmarkDetails?][]>}
     */
    getExtMarks(ns_id: number, start: [number, number] | number, end: [number, number] | number, opts?: {
      details?: boolean
      limit?: number
    }): Promise<[number, number, number, ExtmarkDetails?][]>

    /**
     * Creates or updates an extmark by notification, `:h nvim_buf_set_extmark`.
     *
     * @param {number} ns_id
     * @param {number} line
     * @param {number} col
     * @param {ExtmarkOptions} opts
     * @returns {void}
     */
    setExtMark(ns_id: number, line: number, col: number, opts?: ExtmarkOptions): void

    /**
     * Add sign to buffer by notification.
     *
     * @param {SignPlaceOption} sign
     */
    placeSign(sign: SignPlaceOption): void

    /**
     * Unplace signs by notification
     */
    unplaceSign(opts: SignUnplaceOption): void

    /**
     * Get signs by group name or id and lnum.
     *
     * @param {SignPlacedOption} opts
     */
    getSigns(opts: SignPlacedOption): Promise<SignItem[]>

    /**
     * Get highlight items by namespace (end inclusive).
     *
     * @param {string} ns Namespace key or id.
     * @param {number} start 0 based line number, default to 0.
     * @param {number} end 0 based line number, default to -1.
     */
    getHighlights(ns: string, start?: number, end?: number): Promise<HighlightItem[]>

    /**
     * Update namespaced highlights in range by notification.
     * Priority default to 0 on vim and 4096 on neovim.
     * Note: timer used for whole buffer highlights for better performance.
     *
     * @param {string} ns Namespace key.
     * @param {HighlightItem[]} highlights Highlight items.
     * @param {HighlightOption} opts Highlight options.
     */
    updateHighlights(ns: string, highlights: ExtendedHighlightItem[], opts?: HighlightOption): void

    /**
     * Gets a map of buffer-local |user-commands|.
     *
     * **Note:** works on neovim only.
     */
    getCommands(options?: {}): Promise<Object>

    /**
     * Get lines of buffer, get all lines by default.
     */
    getLines(opts?: { start: number, end: number, strictIndexing?: boolean }): Promise<string[]>

    /**
     * Set lines of buffer given indices use request.
     */
    setLines(lines: string[], opts?: { start: number, end: number, strictIndexing?: boolean }): Promise<void>

    /**
     * Set lines of buffer given indices use notification.
     */
    setLines(lines: string[], opts: { start: number, end: number, strictIndexing?: boolean }, isNotify: true): void

    /**
     * Set virtual text for a line
     *
     * @public
     * @deprecated Use `setExtMark()` instead.
     * @param {number} src_id - Source group to use or 0 to use a new group, or -1
     * @param {number} line - Line to annotate with virtual text (zero-indexed)
     * @param {Chunk[]} chunks - List with [text, hl_group]
     * @param {[index} opts
     * @returns {Promise<number>}
     */
    setVirtualText(src_id: number, line: number, chunks: [string, string][], opts?: { [index: string]: any }): Promise<number>

    /**
     * Append a string or list of lines to end of buffer
     */
    append(lines: string[] | string): Promise<void>

    /**
     * Get buffer name.
     */
    name: Promise<string>

    /**
     * Set buffer name.
     */
    setName(name: string): Promise<void>

    /**
     * Check if buffer valid.
     */
    valid: Promise<boolean>

    /**
     * Get mark position given mark name
     *
     * **Note:** works on neovim only.
     */
    mark(name: string): Promise<[number, number]>

    /**
     * Gets a list of buffer-local |mapping| definitions.
     *
     * @return Array of maparg()-like dictionaries describing mappings.
     * The "buffer" key holds the associated buffer handle.
     */
    getKeymap(mode: string): Promise<object[]>

    /**
     * Check if buffer loaded.
     */
    loaded: Promise<boolean>

    /**
     * Returns the byte offset for a line.
     *
     * Line 1 (index=0) has offset 0. UTF-8 bytes are counted. EOL is
     * one byte. 'fileformat' and 'fileencoding' are ignored. The
     * line index just after the last line gives the total byte-count
     * of the buffer. A final EOL byte is counted if it would be
     * written, see 'eol'.
     *
     * Unlike |line2byte()|, throws error for out-of-bounds indexing.
     * Returns -1 for unloaded buffer.
     *
     * @return {Number} Integer byte offset, or -1 for unloaded buffer.
     */
    getOffset(index: number): Promise<number>

    /**
     * Adds a highlight to buffer, checkout |nvim_buf_add_highlight|.
     *
     * Note: when `srcId = 0`, request is made for new `srcId`, otherwire, use notification.
     * Note: `hlGroup` as empty string is not supported.
     *
     * @deprecated use `highlightRanges()` instead.
     */
    addHighlight(opts: BufferHighlight): Promise<number | null>

    /**
     * Clear highlights of specified lins.
     *
     * @deprecated use clearNamespace() instead.
     */
    clearHighlight(args?: BufferClearHighlight)

    /**
     * Add highlight to ranges by notification, works on both vim & neovim.
     *
     * Works on neovim and `workspace.isVim && workspace.env.textprop` is true
     *
     * @param {string | number} srcId Unique key or namespace number.
     * @param {string} hlGroup Highlight group.
     * @param {Range[]} ranges List of highlight ranges
     */
    highlightRanges(srcId: string | number, hlGroup: string, ranges: Range[]): void

    /**
     * Clear namespace by id or name by notification, works on both vim & neovim.
     *
     * Works on neovim and `workspace.isVim && workspace.env.textprop` is true
     *
     * @param key Unique key or namespace number, use -1 for all namespaces
     * @param lineStart Start of line, 0 based, default to 0.
     * @param lineEnd End of line, 0 based, default to -1.
     */
    clearNamespace(key: number | string, lineStart?: number, lineEnd?: number)
  }

  export interface Window extends BaseApi<Window> {
    /**
     * The windowid that not change within a Vim session
     */
    id: number

    /**
     * Buffer in window.
     */
    buffer: Promise<Buffer>

    /**
     * Tabpage contains window.
     */
    tabpage: Promise<Tabpage>

    /**
     * Cursor position as [line, col], 1 based.
     */
    cursor: Promise<[number, number]>

    /**
     * Window height.
     */
    height: Promise<number>

    /**
     * Window width.
     */
    width: Promise<number>

    /**
     * Set cursor position by request.
     */
    setCursor(pos: [number, number]): Promise<void>

    /**
     * Set cursor position by notification.
     */
    setCursor(pos: [number, number], isNotify: true): void

    /**
     * Set height
     */
    setHeight(height: number): Promise<void>

    /**
     * Set height by notification.
     */
    setHeight(height: number, isNotify: true): void

    /**
     * Set width.
     */
    setWidth(width: number): Promise<void>

    /**
     * Set width by notification.
     */
    setWidth(width: number, isNotify: true): void

    /**
     * Get window position, not work with vim8's popup.
     */
    position: Promise<[number, number]>

    /** 0-indexed, on-screen window position(row) in display cells. */
    row: Promise<number>

    /** 0-indexed, on-screen window position(col) in display cells. */
    col: Promise<number>

    /**
     * Check if window valid.
     */
    valid: Promise<boolean>

    /**
     * Get window number, throws for invalid window.
     */
    number: Promise<number>

    /**
     * Config float window with options.
     *
     * **Note:** works on neovim only.
     */
    setConfig(options: NvimFloatOptions): Promise<void>

    /**
     * Config float window with options by send notification.
     *
     * **Note:** works on neovim only.
     */
    setConfig(options: NvimFloatOptions, isNotify: true): void

    /**
     * Gets window configuration.
     *
     * **Note:** works on neovim only.
     *
     * @returns Map defining the window configuration, see |nvim_open_win()|
     */
    getConfig(): Promise<NvimFloatOptions>

    /**
     * Close window by send request.
     */
    close(force: boolean): Promise<void>

    /**
     * Close window by send notification.
     */
    close(force: boolean, isNotify: true): void

    /**
     * Add highlight to ranges by request (matchaddpos is used)
     *
     * @return {Promise<number[]>} match ids.
     */
    highlightRanges(hlGroup: string, ranges: Range[], priority?: number): Promise<number[]>

    /**
     * Add highlight to ranges by notification (matchaddpos is used)
     */
    highlightRanges(hlGroup: string, ranges: Range[], priority: number, isNotify: true): void

    /**
     * Clear match of highlight group by send notification.
     */
    clearMatchGroup(hlGroup: string): void

    /**
     * Clear match of match ids by send notification.
     */
    clearMatches(ids: number[]): void
  }

  export interface Tabpage extends BaseApi<Tabpage> {
    /**
     * tabpage number.
     */
    number: Promise<number>

    /**
     * Is current tabpage valid.
     */
    valid: Promise<boolean>

    /**
     * Returns all windows of tabpage.
     */
    windows: Promise<Window[]>

    /**
     * Current window of tabpage.
     */
    window: Promise<Window>
  }
  // }}

  // vscode-uri {{
  export interface UriComponents {
    scheme: string
    authority: string
    path: string
    query: string
    fragment: string
  }
  /**
   * Uniform Resource Identifier (URI) http://tools.ietf.org/html/rfc3986.
   * This class is a simple parser which creates the basic component parts
   * (http://tools.ietf.org/html/rfc3986#section-3) with minimal validation
   * and encoding.
   *
   * ```txt
   *       foo://example.com:8042/over/there?name=ferret#nose
   *       \_/   \______________/\_________/ \_________/ \__/
   *        |           |            |            |        |
   *     scheme     authority       path        query   fragment
   *        |   _____________________|__
   *       / \ /                        \
   *       urn:example:animal:ferret:nose
   * ```
   */
  export class Uri implements UriComponents {
    static isUri(thing: any): thing is Uri
    /**
     * scheme is the 'http' part of 'http://www.msft.com/some/path?query#fragment'.
     * The part before the first colon.
     */
    readonly scheme: string
    /**
     * authority is the 'www.msft.com' part of 'http://www.msft.com/some/path?query#fragment'.
     * The part between the first double slashes and the next slash.
     */
    readonly authority: string
    /**
     * path is the '/some/path' part of 'http://www.msft.com/some/path?query#fragment'.
     */
    readonly path: string
    /**
     * query is the 'query' part of 'http://www.msft.com/some/path?query#fragment'.
     */
    readonly query: string
    /**
     * fragment is the 'fragment' part of 'http://www.msft.com/some/path?query#fragment'.
     */
    readonly fragment: string
    /**
     * @internal
     */
    protected constructor(scheme: string, authority?: string, path?: string, query?: string, fragment?: string, _strict?: boolean)
    /**
     * @internal
     */
    protected constructor(components: UriComponents)
    /**
     * Returns a string representing the corresponding file system path of this URI.
     * Will handle UNC paths, normalizes windows drive letters to lower-case, and uses the
     * platform specific path separator.
     *
     * * Will *not* validate the path for invalid characters and semantics.
     * * Will *not* look at the scheme of this URI.
     * * The result shall *not* be used for display purposes but for accessing a file on disk.
     *
     *
     * The *difference* to `URI#path` is the use of the platform specific separator and the handling
     * of UNC paths. See the below sample of a file-uri with an authority (UNC path).
     *
     * ```ts
            const u = URI.parse('file://server/c$/folder/file.txt')
            u.authority === 'server'
            u.path === '/shares/c$/file.txt'
            u.fsPath === '\\server\c$\folder\file.txt'
        ```
     *
     * Using `URI#path` to read a file (using fs-apis) would not be enough because parts of the path,
     * namely the server name, would be missing. Therefore `URI#fsPath` exists - it's sugar to ease working
     * with URIs that represent files on disk (`file` scheme).
     */
    readonly fsPath: string
    with(change: {
      scheme?: string
      authority?: string | null
      path?: string | null
      query?: string | null
      fragment?: string | null
    }): Uri
    /**
     * Creates a new URI from a string, e.g. `http://www.msft.com/some/path`,
     * `file:///usr/home`, or `scheme:with/path`.
     *
     * @param value A string which represents an URI (see `URI#toString`).
     */
    static parse(value: string, _strict?: boolean): Uri
    /**
     * Creates a new URI from a file system path, e.g. `c:\my\files`,
     * `/usr/home`, or `\\server\share\some\path`.
     *
     * The *difference* between `URI#parse` and `URI#file` is that the latter treats the argument
     * as path, not as stringified-uri. E.g. `URI.file(path)` is **not the same as**
     * `URI.parse('file://' + path)` because the path might contain characters that are
     * interpreted (# and ?). See the following sample:
     * ```ts
        const good = URI.file('/coding/c#/project1');
        good.scheme === 'file';
        good.path === '/coding/c#/project1';
        good.fragment === '';
        const bad = URI.parse('file://' + '/coding/c#/project1');
        bad.scheme === 'file';
        bad.path === '/coding/c'; // path is now broken
        bad.fragment === '/project1';
        ```
     *
     * @param path A file system path (see `URI#fsPath`)
     */
    static file(path: string): Uri
    static from(components: {
      scheme: string
      authority?: string
      path?: string
      query?: string
      fragment?: string
    }): Uri
    /**
     * Creates a string representation for this URI. It's guaranteed that calling
     * `URI.parse` with the result of this function creates an URI which is equal
     * to this URI.
     *
     * * The result shall *not* be used for display purposes but for externalization or transport.
     * * The result will be encoded using the percentage encoding and encoding happens mostly
     * ignore the scheme-specific encoding rules.
     *
     * @param skipEncoding Do not encode the result, default is `false`
     */
    toString(skipEncoding?: boolean): string
    toJSON(): UriComponents
  }
  // }}

  // vim interfaces {{
  /**
   * See `:h complete-items`
   */
  export interface VimCompleteItem {
    word: string
    abbr?: string
    menu?: string
    info?: string
    kind?: string
    icase?: number
    equal?: number
    dup?: number
    empty?: number
    user_data?: string
  }

  export interface LocationListItem {
    bufnr: number
    lnum: number
    end_lnum: number
    col: number
    end_col: number
    text: string
    type: string
  }

  export interface QuickfixItem {
    uri?: string
    module?: string
    range?: Range
    text?: string
    type?: string
    filename?: string
    bufnr?: number
    lnum?: number
    end_lnum?: number
    col?: number
    end_col?: number
    valid?: boolean
    nr?: number
  }
  // }}

  // provider interfaces {{
  /**
   * A provider result represents the values a provider, like the [`HoverProvider`](#HoverProvider),
   * may return. For once this is the actual result type `T`, like `Hover`, or a thenable that resolves
   * to that type `T`. In addition, `null` and `undefined` can be returned - either directly or from a
   * thenable.
   *
   * The snippets below are all valid implementations of the [`HoverProvider`](#HoverProvider):
   *
   * ```ts
   * let a: HoverProvider = {
   *   provideHover(doc, pos, token): ProviderResult<Hover> {
   *     return new Hover('Hello World')
   *   }
   * }
   *
   * let b: HoverProvider = {
   *   provideHover(doc, pos, token): ProviderResult<Hover> {
   *     return new Promise(resolve => {
   *       resolve(new Hover('Hello World'))
   *      })
   *   }
   * }
   *
   * let c: HoverProvider = {
   *   provideHover(doc, pos, token): ProviderResult<Hover> {
   *     return; // undefined
   *   }
   * }
   * ```
   */
  export type ProviderResult<T> =
    | T
    | undefined
    | null
    | Thenable<T | undefined | null>

  /**
   * Supported provider names.
   */
  export type ProviderName = 'rename' | 'onTypeEdit' | 'documentLink' | 'documentColor'
    | 'foldingRange' | 'format' | 'codeAction' | 'workspaceSymbols' | 'formatRange' | 'formatOnType'
    | 'hover' | 'signature' | 'documentSymbol' | 'documentHighlight' | 'definition'
    | 'declaration' | 'typeDefinition' | 'reference' | 'implementation'
    | 'codeLens' | 'selectionRange' | 'callHierarchy' | 'semanticTokens' | 'linkedEditing'

  /**
   * The completion item provider interface defines the contract between extensions and
   * [IntelliSense](https://code.visualstudio.com/docs/editor/intellisense).
   *
   * Providers can delay the computation of the [`detail`](#CompletionItem.detail)
   * and [`documentation`](#CompletionItem.documentation) properties by implementing the
   * [`resolveCompletionItem`](#CompletionItemProvider.resolveCompletionItem)-function. However, properties that
   * are needed for the initial sorting and filtering, like `sortText`, `filterText`, `insertText`, and `range`, must
   * not be changed during resolve.
   *
   * Providers are asked for completions either explicitly by a user gesture or -depending on the configuration-
   * implicitly when typing words or trigger characters.
   */
  export interface CompletionItemProvider {
    /**
     * Provide completion items for the given position and document.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param token A cancellation token.
     * @param context How the completion was triggered.
     *
     * @return An array of completions, a [completion list](#CompletionList), or a thenable that resolves to either.
     * The lack of a result can be signaled by returning `undefined`, `null`, or an empty array.
     */
    provideCompletionItems(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken,
      context?: CompletionContext
    ): ProviderResult<CompletionItem[] | CompletionList>

    /**
     * Given a completion item fill in more data, like [doc-comment](#CompletionItem.documentation)
     * or [details](#CompletionItem.detail).
     *
     * The editor will only resolve a completion item once.
     *
     * @param item A completion item currently active in the UI.
     * @param token A cancellation token.
     * @return The resolved completion item or a thenable that resolves to of such. It is OK to return the given
     * `item`. When no result is returned, the given `item` will be used.
     */
    resolveCompletionItem?(
      item: CompletionItem,
      token: CancellationToken
    ): ProviderResult<CompletionItem>
  }

  /**
   * The hover provider interface defines the contract between extensions and
   * the [hover](https://code.visualstudio.com/docs/editor/intellisense)-feature.
   */
  export interface HoverProvider {
    /**
     * Provide a hover for the given position and document. Multiple hovers at the same
     * position will be merged by the editor. A hover can have a range which defaults
     * to the word range at the position when omitted.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param token A cancellation token.
     * @return A hover or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    provideHover(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken
    ): ProviderResult<Hover>
  }

  /**
   * The definition provider interface defines the contract between extensions and
   * the [go to definition](https://code.visualstudio.com/docs/editor/editingevolved#_go-to-definition)
   * and peek definition features.
   */
  export interface DefinitionProvider {
    /**
     * Provide the definition of the symbol at the given position and document.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param token A cancellation token.
     * @return A definition or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    provideDefinition(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken
    ): ProviderResult<Definition | DefinitionLink[]>
  }

  /**
   * The definition provider interface defines the contract between extensions and
   * the [go to definition](https://code.visualstudio.com/docs/editor/editingevolved#_go-to-definition)
   * and peek definition features.
   */
  export interface DeclarationProvider {
    /**
     * Provide the declaration of the symbol at the given position and document.
     */
    provideDeclaration(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken
    ): ProviderResult<Definition | DefinitionLink[]>
  }

  /**
   * The signature help provider interface defines the contract between extensions and
   * the [parameter hints](https://code.visualstudio.com/docs/editor/intellisense)-feature.
   */
  export interface SignatureHelpProvider {
    /**
     * Provide help for the signature at the given position and document.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param token A cancellation token.
     * @return Signature help or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    provideSignatureHelp(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken,
      context: SignatureHelpContext
    ): ProviderResult<SignatureHelp>
  }

  /**
   * The type definition provider defines the contract between extensions and
   * the go to type definition feature.
   */
  export interface TypeDefinitionProvider {
    /**
     * Provide the type definition of the symbol at the given position and document.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param token A cancellation token.
     * @return A definition or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    provideTypeDefinition(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken
    ): ProviderResult<Definition | DefinitionLink[]>
  }

  /**
   * Value-object that contains additional information when
   * requesting references.
   */
  export interface ReferenceContext {
    /**
     * Include the declaration of the current symbol.
     */
    includeDeclaration: boolean
  }

  /**
   * The reference provider interface defines the contract between extensions and
   * the [find references](https://code.visualstudio.com/docs/editor/editingevolved#_peek)-feature.
   */
  export interface ReferenceProvider {
    /**
     * Provide a set of project-wide references for the given position and document.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param context
     * @param token A cancellation token.
     * @return An array of locations or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideReferences(
      document: LinesTextDocument,
      position: Position,
      context: ReferenceContext,
      token: CancellationToken
    ): ProviderResult<Location[]>
  }

  /**
   * Folding context (for future use)
   */
  export interface FoldingContext {}

  /**
   * The folding range provider interface defines the contract between extensions and
   * [Folding](https://code.visualstudio.com/docs/editor/codebasics#_folding) in the editor.
   */
  export interface FoldingRangeProvider {
    /**
     * Returns a list of folding ranges or null and undefined if the provider
     * does not want to participate or was cancelled.
     *
     * @param document The document in which the command was invoked.
     * @param context Additional context information (for future use)
     * @param token A cancellation token.
     */
    provideFoldingRanges(
      document: LinesTextDocument,
      context: FoldingContext,
      token: CancellationToken
    ): ProviderResult<FoldingRange[]>
  }

  /**
   * The document symbol provider interface defines the contract between extensions and
   * the [go to symbol](https://code.visualstudio.com/docs/editor/editingevolved#_go-to-symbol)-feature.
   */
  export interface DocumentSymbolProvider {
    /**
     * Provide symbol information for the given document.
     *
     * @param document The document in which the command was invoked.
     * @param token A cancellation token.
     * @return An array of document highlights or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideDocumentSymbols(
      document: LinesTextDocument,
      token: CancellationToken
    ): ProviderResult<SymbolInformation[] | DocumentSymbol[]>
  }

  /**
   * The implementation provider interface defines the contract between extensions and
   * the go to implementation feature.
   */
  export interface ImplementationProvider {
    /**
     * Provide the implementations of the symbol at the given position and document.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param token A cancellation token.
     * @return A definition or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    provideImplementation(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken
    ): ProviderResult<Definition | DefinitionLink[]>
  }

  /**
   * The workspace symbol provider interface defines the contract between extensions and
   * the [symbol search](https://code.visualstudio.com/docs/editor/editingevolved#_open-symbol-by-name)-feature.
   */
  export interface WorkspaceSymbolProvider {
    /**
     * Project-wide search for a symbol matching the given query string. It is up to the provider
     * how to search given the query string, like substring, indexOf etc. To improve performance implementors can
     * skip the [location](#SymbolInformation.location) of symbols and implement `resolveWorkspaceSymbol` to do that
     * later.
     *
     * The `query`-parameter should be interpreted in a *relaxed way* as the editor will apply its own highlighting
     * and scoring on the results. A good rule of thumb is to match case-insensitive and to simply check that the
     * characters of *query* appear in their order in a candidate symbol. Don't use prefix, substring, or similar
     * strict matching.
     *
     * @param query A non-empty query string.
     * @param token A cancellation token.
     * @return An array of document highlights or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideWorkspaceSymbols(
      query: string,
      token: CancellationToken
    ): ProviderResult<SymbolInformation[]>

    /**
     * Given a symbol fill in its [location](#SymbolInformation.location). This method is called whenever a symbol
     * is selected in the UI. Providers can implement this method and return incomplete symbols from
     * [`provideWorkspaceSymbols`](#WorkspaceSymbolProvider.provideWorkspaceSymbols) which often helps to improve
     * performance.
     *
     * @param symbol The symbol that is to be resolved. Guaranteed to be an instance of an object returned from an
     * earlier call to `provideWorkspaceSymbols`.
     * @param token A cancellation token.
     * @return The resolved symbol or a thenable that resolves to that. When no result is returned,
     * the given `symbol` is used.
     */
    resolveWorkspaceSymbol?(
      symbol: SymbolInformation,
      token: CancellationToken
    ): ProviderResult<SymbolInformation>
  }

  /**
   * The rename provider interface defines the contract between extensions and
   * the [rename](https://code.visualstudio.com/docs/editor/editingevolved#_rename-symbol)-feature.
   */
  export interface RenameProvider {
    /**
     * Provide an edit that describes changes that have to be made to one
     * or many resources to rename a symbol to a different name.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param newName The new name of the symbol. If the given name is not valid, the provider must return a rejected promise.
     * @param token A cancellation token.
     * @return A workspace edit or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    provideRenameEdits(
      document: LinesTextDocument,
      position: Position,
      newName: string,
      token: CancellationToken
    ): ProviderResult<WorkspaceEdit>

    /**
     * Optional function for resolving and validating a position *before* running rename. The result can
     * be a range or a range and a placeholder text. The placeholder text should be the identifier of the symbol
     * which is being renamed - when omitted the text in the returned range is used.
     *
     * @param document The document in which rename will be invoked.
     * @param position The position at which rename will be invoked.
     * @param token A cancellation token.
     * @return The range or range and placeholder text of the identifier that is to be renamed. The lack of a result can signaled by returning `undefined` or `null`.
     */
    prepareRename?(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken
    ): ProviderResult<Range | { range: Range; placeholder: string }>
  }

  /**
   * The document formatting provider interface defines the contract between extensions and
   * the formatting-feature.
   */
  export interface DocumentFormattingEditProvider {
    /**
     * Provide formatting edits for a whole document.
     *
     * @param document The document in which the command was invoked.
     * @param options Options controlling formatting.
     * @param token A cancellation token.
     * @return A set of text edits or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideDocumentFormattingEdits(
      document: LinesTextDocument,
      options: FormattingOptions,
      token: CancellationToken
    ): ProviderResult<TextEdit[]>
  }

  /**
   * The document formatting provider interface defines the contract between extensions and
   * the formatting-feature.
   */
  export interface DocumentRangeFormattingEditProvider {
    /**
     * Provide formatting edits for a range in a document.
     *
     * The given range is a hint and providers can decide to format a smaller
     * or larger range. Often this is done by adjusting the start and end
     * of the range to full syntax nodes.
     *
     * @param document The document in which the command was invoked.
     * @param range The range which should be formatted.
     * @param options Options controlling formatting.
     * @param token A cancellation token.
     * @return A set of text edits or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideDocumentRangeFormattingEdits(
      document: LinesTextDocument,
      range: Range,
      options: FormattingOptions,
      token: CancellationToken
    ): ProviderResult<TextEdit[]>
  }

  /**
   * The code action interface defines the contract between extensions and
   * the [light bulb](https://code.visualstudio.com/docs/editor/editingevolved#_code-action) feature.
   *
   * A code action can be any command that is [known](#commands.getCommands) to the system.
   */
  export interface CodeActionProvider<T extends CodeAction = CodeAction> {
    /**
     * Provide commands for the given document and range.
     *
     * @param document The document in which the command was invoked.
     * @param range The selector or range for which the command was invoked. This will always be a selection if
     * there is a currently active editor.
     * @param context Context carrying additional information.
     * @param token A cancellation token.
     * @return An array of commands, quick fixes, or refactorings or a thenable of such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideCodeActions(
      document: LinesTextDocument,
      range: Range,
      context: CodeActionContext,
      token: CancellationToken
    ): ProviderResult<(Command | CodeAction)[]>

    /**
     * Given a code action fill in its [`edit`](#CodeAction.edit)-property. Changes to
     * all other properties, like title, are ignored. A code action that has an edit
     * will not be resolved.
     *
     * @param codeAction A code action.
     * @param token A cancellation token.
     * @return The resolved code action or a thenable that resolves to such. It is OK to return the given
     * `item`. When no result is returned, the given `item` will be used.
     */
    resolveCodeAction?(codeAction: T, token: CancellationToken): ProviderResult<T>
  }

  /**
   * Metadata about the type of code actions that a [CodeActionProvider](#CodeActionProvider) providers
   */
  export interface CodeActionProviderMetadata {
    /**
     * [CodeActionKinds](#CodeActionKind) that this provider may return.
     *
     * The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the provider
     * may list our every specific kind they provide, such as `CodeActionKind.Refactor.Extract.append('function`)`
     */
    readonly providedCodeActionKinds?: ReadonlyArray<string>
  }

  /**
   * The document highlight provider interface defines the contract between extensions and
   * the word-highlight-feature.
   */
  export interface DocumentHighlightProvider {

    /**
     * Provide a set of document highlights, like all occurrences of a variable or
     * all exit-points of a function.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param token A cancellation token.
     * @return An array of document highlights or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideDocumentHighlights(
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken
    ): ProviderResult<DocumentHighlight[]>
  }

  /**
   * The document link provider defines the contract between extensions and feature of showing
   * links in the editor.
   */
  export interface DocumentLinkProvider {

    /**
     * Provide links for the given document. Note that the editor ships with a default provider that detects
     * `http(s)` and `file` links.
     *
     * @param document The document in which the command was invoked.
     * @param token A cancellation token.
     * @return An array of [document links](#DocumentLink) or a thenable that resolves to such. The lack of a result
     * can be signaled by returning `undefined`, `null`, or an empty array.
     */
    provideDocumentLinks(document: LinesTextDocument, token: CancellationToken): ProviderResult<DocumentLink[]>

    /**
     * Given a link fill in its [target](#DocumentLink.target). This method is called when an incomplete
     * link is selected in the UI. Providers can implement this method and return incomple links
     * (without target) from the [`provideDocumentLinks`](#DocumentLinkProvider.provideDocumentLinks) method which
     * often helps to improve performance.
     *
     * @param link The link that is to be resolved.
     * @param token A cancellation token.
     */
    resolveDocumentLink?(link: DocumentLink, token: CancellationToken): ProviderResult<DocumentLink>
  }

  /**
   * A code lens provider adds [commands](#Command) to source text. The commands will be shown
   * as dedicated horizontal lines in between the source text.
   */
  export interface CodeLensProvider {

    /**
     * Compute a list of [lenses](#CodeLens). This call should return as fast as possible and if
     * computing the commands is expensive implementors should only return code lens objects with the
     * range set and implement [resolve](#CodeLensProvider.resolveCodeLens).
     *
     * @param document The document in which the command was invoked.
     * @param token A cancellation token.
     * @return An array of code lenses or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideCodeLenses(document: LinesTextDocument, token: CancellationToken): ProviderResult<CodeLens[]>

    /**
     * This function will be called for each visible code lens, usually when scrolling and after
     * calls to [compute](#CodeLensProvider.provideCodeLenses)-lenses.
     *
     * @param codeLens code lens that must be resolved.
     * @param token A cancellation token.
     * @return The given, resolved code lens or thenable that resolves to such.
     */
    resolveCodeLens?(codeLens: CodeLens, token: CancellationToken): ProviderResult<CodeLens>
  }

  /**
   * The document formatting provider interface defines the contract between extensions and
   * the formatting-feature.
   */
  export interface OnTypeFormattingEditProvider {

    /**
     * Provide formatting edits after a character has been typed.
     *
     * The given position and character should hint to the provider
     * what range the position to expand to, like find the matching `{`
     * when `}` has been entered.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param ch The character that has been typed.
     * @param options Options controlling formatting.
     * @param token A cancellation token.
     * @return A set of text edits or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined`, `null`, or an empty array.
     */
    provideOnTypeFormattingEdits(document: LinesTextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]>
  }

  /**
   * The document color provider defines the contract between extensions and feature of
   * picking and modifying colors in the editor.
   */
  export interface DocumentColorProvider {

    /**
     * Provide colors for the given document.
     *
     * @param document The document in which the command was invoked.
     * @param token A cancellation token.
     * @return An array of [color information](#ColorInformation) or a thenable that resolves to such. The lack of a result
     * can be signaled by returning `undefined`, `null`, or an empty array.
     */
    provideDocumentColors(document: LinesTextDocument, token: CancellationToken): ProviderResult<ColorInformation[]>

    /**
     * Provide [representations](#ColorPresentation) for a color.
     *
     * @param color The color to show and insert.
     * @param context A context object with additional information
     * @param token A cancellation token.
     * @return An array of color presentations or a thenable that resolves to such. The lack of a result
     * can be signaled by returning `undefined`, `null`, or an empty array.
     */
    provideColorPresentations(color: Color, context: { document: LinesTextDocument; range: Range }, token: CancellationToken): ProviderResult<ColorPresentation[]>
  }

  export interface TextDocumentContentProvider {

    /**
     * An event to signal a resource has changed.
     */
    onDidChange?: Event<Uri>

    /**
     * Provide textual content for a given uri.
     *
     * The editor will use the returned string-content to create a readonly
     * [document](#LinesTextDocument). Resources allocated should be released when
     * the corresponding document has been [closed](#workspace.onDidCloseTextDocument).
     *
     * @param uri An uri which scheme matches the scheme this provider was [registered](#workspace.registerTextDocumentContentProvider) for.
     * @param token A cancellation token.
     * @return A string or a thenable that resolves to such.
     */
    provideTextDocumentContent(uri: Uri, token: CancellationToken): ProviderResult<string>
  }

  export interface SelectionRangeProvider {
    /**
     * Provide selection ranges starting at a given position. The first range must [contain](#Range.contains)
     * position and subsequent ranges must contain the previous range.
     */
    provideSelectionRanges(document: LinesTextDocument, positions: Position[], token: CancellationToken): ProviderResult<SelectionRange[]>
  }

  /**
   * The call hierarchy provider interface describes the contract between extensions
   * and the call hierarchy feature which allows to browse calls and caller of function,
   * methods, constructor etc.
   */
  export interface CallHierarchyProvider {

    /**
     * Bootstraps call hierarchy by returning the item that is denoted by the given document
     * and position. This item will be used as entry into the call graph. Providers should
     * return `undefined` or `null` when there is no item at the given location.
     *
     * @param document The document in which the command was invoked.
     * @param position The position at which the command was invoked.
     * @param token A cancellation token.
     * @returns A call hierarchy item or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    prepareCallHierarchy(document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<CallHierarchyItem | CallHierarchyItem[]>

    /**
     * Provide all incoming calls for an item, e.g all callers for a method. In graph terms this describes directed
     * and annotated edges inside the call graph, e.g the given item is the starting node and the result is the nodes
     * that can be reached.
     *
     * @param item The hierarchy item for which incoming calls should be computed.
     * @param token A cancellation token.
     * @returns A set of incoming calls or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    provideCallHierarchyIncomingCalls(item: CallHierarchyItem, token: CancellationToken): ProviderResult<CallHierarchyIncomingCall[]>

    /**
     * Provide all outgoing calls for an item, e.g call calls to functions, methods, or constructors from the given item. In
     * graph terms this describes directed and annotated edges inside the call graph, e.g the given item is the starting
     * node and the result is the nodes that can be reached.
     *
     * @param item The hierarchy item for which outgoing calls should be computed.
     * @param token A cancellation token.
     * @returns A set of outgoing calls or a thenable that resolves to such. The lack of a result can be
     * signaled by returning `undefined` or `null`.
     */
    provideCallHierarchyOutgoingCalls(item: CallHierarchyItem, token: CancellationToken): ProviderResult<CallHierarchyOutgoingCall[]>
  }

  /**
   * The document semantic tokens provider interface defines the contract between extensions and
   * semantic tokens.
   */
  export interface DocumentSemanticTokensProvider {
    /**
     * An optional event to signal that the semantic tokens from this provider have changed.
     */
    // TODO: SemantiTokens
    onDidChangeSemanticTokens?: Event<void>

    /**
     * Tokens in a file are represented as an array of integers. The position of each token is expressed relative to
     * the token before it, because most tokens remain stable relative to each other when edits are made in a file.
     *
     * ---
     * In short, each token takes 5 integers to represent, so a specific token `i` in the file consists of the following array indices:
     *
     * - at index `5*i`   - `deltaLine`: token line number, relative to the previous token
     * - at index `5*i+1` - `deltaStart`: token start character, relative to the previous token (relative to 0 or the previous token's start if they are on the same line)
     * - at index `5*i+2` - `length`: the length of the token. A token cannot be multiline.
     * - at index `5*i+3` - `tokenType`: will be looked up in `SemanticTokensLegend.tokenTypes`. We currently ask that `tokenType` < 65536.
     * - at index `5*i+4` - `tokenModifiers`: each set bit will be looked up in `SemanticTokensLegend.tokenModifiers`
     *
     * ---
     * ### How to encode tokens
     *
     * Here is an example for encoding a file with 3 tokens in a uint32 array:
     * ```
     *    { line: 2, startChar:  5, length: 3, tokenType: "property",  tokenModifiers: ["private", "static"] },
     *    { line: 2, startChar: 10, length: 4, tokenType: "type",      tokenModifiers: [] },
     *    { line: 5, startChar:  2, length: 7, tokenType: "class",     tokenModifiers: [] }
     * ```
     *
     * 1. First of all, a legend must be devised. This legend must be provided up-front and capture all possible token types.
     * For this example, we will choose the following legend which must be passed in when registering the provider:
     * ```
     *    tokenTypes: ['property', 'type', 'class'],
     *    tokenModifiers: ['private', 'static']
     * ```
     *
     * 2. The first transformation step is to encode `tokenType` and `tokenModifiers` as integers using the legend. Token types are looked
     * up by index, so a `tokenType` value of `1` means `tokenTypes[1]`. Multiple token modifiers can be set by using bit flags,
     * so a `tokenModifier` value of `3` is first viewed as binary `0b00000011`, which means `[tokenModifiers[0], tokenModifiers[1]]` because
     * bits 0 and 1 are set. Using this legend, the tokens now are:
     * ```
     *    { line: 2, startChar:  5, length: 3, tokenType: 0, tokenModifiers: 3 },
     *    { line: 2, startChar: 10, length: 4, tokenType: 1, tokenModifiers: 0 },
     *    { line: 5, startChar:  2, length: 7, tokenType: 2, tokenModifiers: 0 }
     * ```
     *
     * 3. The next step is to represent each token relative to the previous token in the file. In this case, the second token
     * is on the same line as the first token, so the `startChar` of the second token is made relative to the `startChar`
     * of the first token, so it will be `10 - 5`. The third token is on a different line than the second token, so the
     * `startChar` of the third token will not be altered:
     * ```
     *    { deltaLine: 2, deltaStartChar: 5, length: 3, tokenType: 0, tokenModifiers: 3 },
     *    { deltaLine: 0, deltaStartChar: 5, length: 4, tokenType: 1, tokenModifiers: 0 },
     *    { deltaLine: 3, deltaStartChar: 2, length: 7, tokenType: 2, tokenModifiers: 0 }
     * ```
     *
     * 4. Finally, the last step is to inline each of the 5 fields for a token in a single array, which is a memory friendly representation:
     * ```
     *    // 1st token,  2nd token,  3rd token
     *    [  2,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0 ]
     * ```
     *
     * @see [SemanticTokensBuilder](#SemanticTokensBuilder) for a helper to encode tokens as integers.
     * *NOTE*: When doing edits, it is possible that multiple edits occur until VS Code decides to invoke the semantic tokens provider.
     * *NOTE*: If the provider cannot temporarily compute semantic tokens, it can indicate this by throwing an error with the message 'Busy'.
     */
    provideDocumentSemanticTokens(document: LinesTextDocument, token: CancellationToken): ProviderResult<SemanticTokens>

    /**
     * Instead of always returning all the tokens in a file, it is possible for a `DocumentSemanticTokensProvider` to implement
     * this method (`provideDocumentSemanticTokensEdits`) and then return incremental updates to the previously provided semantic tokens.
     *
     * ---
     * ### How tokens change when the document changes
     *
     * Suppose that `provideDocumentSemanticTokens` has previously returned the following semantic tokens:
     * ```
     *    // 1st token,  2nd token,  3rd token
     *    [  2,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0 ]
     * ```
     *
     * Also suppose that after some edits, the new semantic tokens in a file are:
     * ```
     *    // 1st token,  2nd token,  3rd token
     *    [  3,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0 ]
     * ```
     * It is possible to express these new tokens in terms of an edit applied to the previous tokens:
     * ```
     *    [  2,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0 ] // old tokens
     *    [  3,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0 ] // new tokens
     *
     *    edit: { start:  0, deleteCount: 1, data: [3] } // replace integer at offset 0 with 3
     * ```
     *
     * *NOTE*: If the provider cannot compute `SemanticTokensEdits`, it can "give up" and return all the tokens in the document again.
     * *NOTE*: All edits in `SemanticTokensEdits` contain indices in the old integers array, so they all refer to the previous result state.
     */
    provideDocumentSemanticTokensEdits?(document: LinesTextDocument, previousResultId: string, token: CancellationToken): ProviderResult<SemanticTokens | SemanticTokensDelta>
  }

  /**
   * The document range semantic tokens provider interface defines the contract between extensions and
   * semantic tokens.
   */
  export interface DocumentRangeSemanticTokensProvider {
    /**
     * @see [provideDocumentSemanticTokens](#DocumentSemanticTokensProvider.provideDocumentSemanticTokens).
     */
    provideDocumentRangeSemanticTokens(document: LinesTextDocument, range: Range, token: CancellationToken): ProviderResult<SemanticTokens>
  }

  export interface LinkedEditingRangeProvider {
    /**
     * For a given position in a document, returns the range of the symbol at the position and all ranges
     * that have the same content. A change to one of the ranges can be applied to all other ranges if the new content
     * is valid. An optional word pattern can be returned with the result to describe valid contents.
     * If no result-specific word pattern is provided, the word pattern from the language configuration is used.
     *
     * @param document The document in which the provider was invoked.
     * @param position The position at which the provider was invoked.
     * @param token A cancellation token.
     * @return A list of ranges that can be edited together
     */
    provideLinkedEditingRanges(document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<LinkedEditingRanges>
  }

  /**
   * The inlay hints provider interface defines the contract between extensions and
   * the inlay hints feature.
   */
  export interface InlayHintsProvider<T extends InlayHint = InlayHint> {

    /**
     * An optional event to signal that inlay hints from this provider have changed.
     */
    onDidChangeInlayHints?: Event<void>

    /**
     * Provide inlay hints for the given range and document.
     *
     * *Note* that inlay hints that are not {@link Range.contains contained} by the given range are ignored.
     *
     * @param document The document in which the command was invoked.
     * @param range The range for which inlay hints should be computed.
     * @param token A cancellation token.
     * @return An array of inlay hints or a thenable that resolves to such.
     */
    provideInlayHints(document: TextDocument, range: Range, token: CancellationToken): ProviderResult<T[]>

    /**
     * Given an inlay hint fill in {@link InlayHint.tooltip tooltip}, {@link InlayHint.textEdits text edits},
     * or complete label {@link InlayHintLabelPart parts}.
     *
     * *Note* that the editor will resolve an inlay hint at most once.
     *
     * @param hint An inlay hint.
     * @param token A cancellation token.
     * @return The resolved inlay hint or a thenable that resolves to such. It is OK to return the given `item`. When no result is returned, the given `item` will be used.
     */
    resolveInlayHint?(hint: T, token: CancellationToken): ProviderResult<T>
  }
  // }}

  // Classes {{
  /**
   * A semantic tokens builder can help with creating a `SemanticTokens` instance
   * which contains delta encoded semantic tokens.
   */
  export class SemanticTokensBuilder {
    constructor(legend?: SemanticTokensLegend)

    /**
     * Add another token.
     *
     * @public
     * @param line The token start line number (absolute value).
     * @param char The token start character (absolute value).
     * @param length The token length in characters.
     * @param tokenType The encoded token type.
     * @param tokenModifiers The encoded token modifiers.
     */
    push(line: number, char: number, length: number, tokenType: number, tokenModifiers?: number): void
    /**
     * Add another token. Use only when providing a legend.
     *
     * @public
     * @param range The range of the token. Must be single-line.
     * @param tokenType The token type.
     * @param tokenModifiers The token modifiers.
     */
    push(range: Range, tokenType: string, tokenModifiers?: string[]): void

    /**
     * Finish and create a `SemanticTokens` instance.
     *
     * @public
     */
    build(resultId?: string): SemanticTokens
  }

  export interface Document {
    readonly buffer: Buffer
    /**
     * Document is attached to vim.
     */
    readonly attached: boolean
    /**
     * Is command line document.
     */
    readonly isCommandLine: boolean
    /**
     * `buftype` option of buffer.
     */
    readonly buftype: string
    /**
     * Text document that synchronized.
     */
    readonly textDocument: LinesTextDocument
    /**
     * Fired when document change.
     */
    readonly onDocumentChange: Event<DidChangeTextDocumentParams>
    /**
     * Get current buffer changedtick.
     */
    readonly changedtick: number
    /**
     * Scheme of document.
     */
    readonly schema: string
    /**
     * Line count of current buffer.
     */
    readonly lineCount: number
    /**
     * Window ID when buffer create, could be -1 when no window associated.
     */
    readonly winid: number
    /**
     * Returns if current document is opended with previewwindow
     */
    readonly previewwindow: boolean
    /**
     * Check if document changed after last synchronize
     */
    readonly dirty: boolean
    /**
     * Buffer number
     */
    readonly bufnr: number
    /**
     * Content of textDocument.
     */
    readonly content: string
    /**
     * Converted filetype.
     */
    readonly filetype: string
    /**
     * Main filetype of buffer, first part when buffer filetype contains dots.
     * Same as filetype most of the time.
     */
    readonly languageId: string
    readonly uri: string
    readonly version: number
    /**
     * Apply text edits to document. `nvim_buf_set_text()` is used when possible
     *
     * @param {TextEdit[]} edits
     * @param {boolean} joinUndo - Join further changes with the previous undo block by `:undojoin`.
     * @param {boolean | Position} move - Move the cursor when true or from custom position.
     * @returns {Promise<void>}
     */
    applyEdits(edits: TextEdit[], joinUndo?: boolean, move?: boolean | Position): Promise<void>

    /**
     * Change individual lines.
     *
     * @param {[number, string][]} lines
     * @returns {void}
     */
    changeLines(lines: [number, string][]): Promise<void>

    /**
     * Get offset from lnum & col
     */
    getOffset(lnum: number, col: number): number

    /**
     * Check string is word.
     */
    isWord(word: string): boolean

    /**
     * Word range at position.
     *
     * @param {Position} position
     * @param {string} extraChars Extra characters that should be keyword.
     * @param {boolean} current Use current lines instead of textDocument, default to true.
     * @returns {Range | null}
     */
    getWordRangeAtPosition(position: Position, extraChars?: string, current?: boolean): Range | null

    /**
     * Get ranges of word in textDocument.
     */
    getSymbolRanges(word: string): Range[]

    /**
     * Get line for buffer
     *
     * @param {number} line 0 based line index.
     * @param {boolean} current Use textDocument lines when false, default to true.
     * @returns {string}
     */
    getline(line: number, current?: boolean): string

    /**
     * Get range of current lines, zero indexed, end exclude.
     */
    getLines(start?: number, end?: number): string[]

    /**
     * Get variable value by key, defined by `b:coc_{key}`
     */
    getVar<T>(key: string, defaultValue?: T): T

    /**
     * Get position from lnum & col
     */
    getPosition(lnum: number, col: number): Position

    /**
     * Adjust col with new valid character before position.
     */
    fixStartcol(position: Position, valids: string[]): number

    /**
     * Get current content text.
     */
    getDocumentContent(): string
  }

  /**
   * Represents a {@link TextEditor text editor}'s {@link TextEditor.options options}.
   */
  export interface TextEditorOptions {
    /**
     * The size in spaces a tab takes. This is used for two purposes:
     *  - the rendering width of a tab character;
     *  - the number of spaces to insert when {@link TextEditorOptions.insertSpaces insertSpaces} is true.
     *
     * When getting a text editor's options, this property will always be a number (resolved).
    */
    tabSize: number
    /**
     * When pressing Tab insert {@link TextEditorOptions.tabSize n} spaces.
     * When getting a text editor's options, this property will always be a boolean (resolved).
     */
    insertSpaces: boolean
  }

  /**
   * Represents an editor that is attached to a {@link TextDocument document}.
   */
  export interface TextEditor {
    /**
     * The tabpagenr of current editor.
     */
    readonly tabpagenr: number
    /**
     * The window id of current editor.
     */
    readonly winid: number
    /**
     * The window number of current editor.
     */
    readonly winnr: number
    /**
     * The document associated with this text editor. The document will be the same for the entire lifetime of this text editor.
     */
    readonly document: Document
    /**
     * The current visible ranges in the editor (vertically).
     * This accounts only for vertical scrolling, and not for horizontal scrolling.
     */
    readonly visibleRanges: readonly Range[]
    /**
     * Text editor options.
     */
    readonly options: TextEditorOptions
  }

  export interface FloatWinConfig {
    maxHeight?: number
    maxWidth?: number
    preferTop?: boolean
    autoHide?: boolean
    offsetX?: number
    title?: string
    border?: number[]
    cursorline?: boolean
    close?: boolean
    highlight?: string
    borderhighlight?: string
    modes?: string[]
    shadow?: boolean
    winblend?: number
    focusable?: boolean
    excludeImages?: boolean
  }

  export interface Documentation {
    /**
     * Filetype used for highlight, markdown is supported.
     */
    filetype: string
    /**
     * Content of document.
     */
    content: string
    /**
     * Byte offset (0 based) that should be undelined.
     */
    active?: [number, number]
    highlights?: HighlightItem[]
  }

  /**
   * Float window factory for create float around current cursor, works on vim and neovim.
   * Use `workspace.floatSupported` to check if float could work.
   *
   * Float windows are automatic reused and hidden on specific events including:
   *  - BufEnter
   *  - InsertEnter
   *  - InsertLeave
   *  - MenuPopupChanged
   *  - CursorMoved
   *  - CursorMovedI
   */
  export class FloatFactory implements Disposable {
    get bufnr(): number | undefined
    get buffer(): Buffer | null
    get window(): Window | null
    activated(): Promise<boolean>

    constructor(nvim: Neovim)

    /**
     * Show documentations in float window/popup around cursor.
     * Window and buffer are reused when possible.
     * Window is closed automatically on change buffer, InsertEnter, CursorMoved and CursorMovedI.
     *
     * @param docs List of documentations.
     * @param config Configuration for floating window/popup.
     */
    show(docs: Documentation[], config?: FloatWinConfig): Promise<void>

    /**
     * Close float window.
     */
    close(): void
    dispose(): void
  }


  /**
   * A file glob pattern to match file paths against. This can either be a glob pattern string
   * (like `**​/*.{ts,js}` or `*.{ts,js}`) or a {@link RelativePattern relative pattern}.
   *
   * Glob patterns can have the following syntax:
   * * `*` to match one or more characters in a path segment
   * * `?` to match on one character in a path segment
   * * `**` to match any number of path segments, including none
   * * `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
   * * `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
   * * `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
   *
   * Note: a backslash (`\`) is not valid within a glob pattern. If you have an existing file
   * path to match against, consider to use the {@link RelativePattern relative pattern} support
   * that takes care of converting any backslash into slash. Otherwise, make sure to convert
   * any backslash to slash when creating the glob pattern.
   */
  export type GlobPattern = string | RelativePattern

  /**
   * A relative pattern is a helper to construct glob patterns that are matched
   * relatively to a base file path. The base path can either be an absolute file
   * path as string or uri or a {@link WorkspaceFolder workspace folder}, which is the
   * preferred way of creating the relative pattern.
   */
  export class RelativePattern {

    /**
     * A base file path to which this pattern will be matched against relatively.
     */
    baseUri: Uri

    /**
     * A file glob pattern like `*.{ts,js}` that will be matched on file paths
     * relative to the base path.
     *
     * Example: Given a base of `/home/work/folder` and a file path of `/home/work/folder/index.js`,
     * the file glob pattern will match on `index.js`.
     */
    pattern: string

    /**
     * Creates a new relative pattern object with a base file path and pattern to match. This pattern
     * will be matched on file paths relative to the base.
     *
     * Example:
     * ```ts
     * const folder = vscode.workspace.workspaceFolders?.[0];
     * if (folder) {
     *
     *   // Match any TypeScript file in the root of this workspace folder
     *   const pattern1 = new vscode.RelativePattern(folder, '*.ts');
     *
     *   // Match any TypeScript file in `someFolder` inside this workspace folder
     *   const pattern2 = new vscode.RelativePattern(folder, 'someFolder/*.ts');
     * }
     * ```
     *
     * @param base A base to which this pattern will be matched against relatively. It is recommended
     * to pass in a {@link WorkspaceFolder workspace folder} if the pattern should match inside the workspace.
     * Otherwise, a uri or string should only be used if the pattern is for a file path outside the workspace.
     * @param pattern A file glob pattern like `*.{ts,js}` that will be matched on paths relative to the base.
     */
    constructor(base: WorkspaceFolder | Uri | string, pattern: string)
  }

  /**
   * Build buffer with lines and highlights
   */
  export class Highlighter {
    constructor(srcId?: number)
    /**
     * Add a line with highlight group.
     */
    addLine(line: string, hlGroup?: string): void
    /**
     * Add lines without highlights.
     */
    addLines(lines: string[]): void
    /**
     * Add text with highlight.
     */
    addText(text: string, hlGroup?: string): void
    /**
     * Get line count
     */
    get length(): number
    /**
     * Render lines to buffer at specified range.
     * Since notifications is used, use `nvim.pauseNotification` & `nvim.resumeNotification`
     * when you need to wait for the request finish.
     *
     * @param {Buffer} buffer
     * @param {number} start
     * @param {number} end
     * @returns {void}
     */
    render(buffer: Buffer, start?: number, end?: number): void
  }

  export interface ListConfiguration {
    get<T>(key: string, defaultValue?: T): T
    previousKey(): string
    nextKey(): string
    dispose(): void
  }

  export interface ListActionOptions {
    /**
     * No prompt stop and window switch when invoked.
     */
    persist?: boolean
    /**
     * Reload list after action invoked.
     */
    reload?: boolean
    /**
     * Support multiple items as execute argument.
     */
    parallel?: boolean
    /**
     * Tab positioned list should be persisted (no window switch) on action invoke.
     */
    tabPersist?: boolean
  }

  export interface CommandTaskOption {
    /**
     * Command to run.
     */
    cmd: string
    /**
     * Arguments of command.
     */
    args: string[]
    /**
     * Current working directory.
     */
    cwd?: string
    env?: NodeJS.ProcessEnv
    /**
     * Runs for each line, return undefined for invalid item.
     */
    onLine: (line: string) => ListItem | undefined
  }

  export abstract class BasicList implements IList {
    /**
     * Unique name, must be provided by implementation class.
     */
    name: string
    /**
     * Default action name invoked by <cr> by default, must be provided by implementation class.
     */
    defaultAction: string
    /**
     * Registered actions.
     */
    readonly actions: ListAction[]
    /**
     * Arguments configuration of list.
     */
    options: ListArgument[]
    protected nvim: Neovim
    protected disposables: Disposable[]
    protected config: ListConfiguration
    constructor(nvim: Neovim)
    /**
     * Should align columns when true.
     */
    get alignColumns(): boolean
    get hlGroup(): string
    get previewHeight(): string
    get splitRight(): boolean
    /**
     * Parse argument string array for argument object from `this.options`.
     * Could be used inside `this.loadItems()`
     */
    protected parseArguments(args: string[]): { [key: string]: string | boolean }
    /**
     * Get configurations of current list
     */
    protected getConfig(): WorkspaceConfiguration
    /**
     * Add an action
     */
    protected addAction(name: string, fn: (item: ListItem, context: ListContext) => ProviderResult<void>, options?: ListActionOptions): void
    /**
     * Add action that support multiple selection.
     */
    protected addMultipleAction(name: string, fn: (item: ListItem[], context: ListContext) => ProviderResult<void>, options?: ListActionOptions): void
    /**
     * Create task from command task option.
     */
    protected createCommandTask(opt: CommandTaskOption): ListTask
    /**
     * Add location related actions, should be called in constructor.
     */
    protected addLocationActions(): void
    protected convertLocation(location: Location | LocationWithLine | string): Promise<Location>
    /**
     * Jump to location
     *
     * @method
     */
    protected jumpTo(location: Location | LocationWithLine | string, command?: string): Promise<void>
    /**
     * Preview location.
     *
     * @method
     */
    protected previewLocation(location: Location, context: ListContext): Promise<void>
    /**
     * Preview lines.
     *
     * @method
     */
    protected preview(options: PreviewOptions, context: ListContext): Promise<void>
    /**
     * Use for syntax highlights, invoked after buffer loaded.
     */
    doHighlight(): void
    /**
     * Invoked for listItems or listTask, could throw error when failed to load.
     */
    abstract loadItems(context: ListContext, token?: CancellationToken): Promise<ListItem[] | ListTask | null | undefined>
  }

  export class Mutex {
    /**
     * Returns true when task is running.
     */
    get busy(): boolean
    /**
     * Resolved release function that must be called after task finish.
     */
    acquire(): Promise<() => void>
    /**
     * Captrue the async task function that ensures to be executed one by one.
     */
    use<T>(f: () => Promise<T>): Promise<T>
  }
  // }}

  // functions {{

  export interface AnsiItem {
    foreground?: string
    background?: string
    bold?: boolean
    italic?: boolean
    underline?: boolean
    text: string
  }

  export interface ParsedUrlQueryInput {
    [key: string]: unknown
  }

  export interface FetchOptions {
    /**
     * Default to 'GET'
     */
    method?: string
    /**
     * Default no timeout
     */
    timeout?: number
    /**
     * Always return buffer instead of parsed response.
     */
    buffer?: boolean
    /**
     * Data send to server.
     */
    data?: string | { [key: string]: any } | Buffer
    /**
     * Plain object added as query of url
     */
    query?: ParsedUrlQueryInput
    headers?: any
    /**
     * User for http basic auth, should use with password
     */
    user?: string
    /**
     * Password for http basic auth, should use with user
     */
    password?: string
  }

  export interface DownloadOptions extends Omit<FetchOptions, 'buffer'> {
    /**
     * Folder that contains downloaded file or extracted files by untar or unzip
     */
    dest: string
    /**
     * Remove the specified number of leading path elements for *untar* only, default to `1`.
     */
    strip?: number
    /**
     * If true, use untar for `.tar.gz` filename
     */
    extract?: boolean | 'untar' | 'unzip'
    onProgress?: (percent: string) => void
  }

  export type ResponseResult = string | Buffer | {
    [name: string]: any
  }

  /**
   * Parse ansi result from string contains ansi characters.
   */
  export function ansiparse(str: string): AnsiItem[]

  /**
   * Send request to server for response, supports:
   *
   * - Send json data and parse json response.
   * - Throw error for failed response statusCode.
   * - Timeout support (no timeout by default).
   * - Send buffer (as data) and receive data (as response).
   * - Proxy support from user configuration & environment.
   * - Redirect support, limited to 3.
   * - Support of gzip & deflate response content.
   *
   * @return Parsed object if response content type is application/json, text if content type starts with `text/`
   */
  export function fetch(url: string, options?: FetchOptions, token?: CancellationToken): Promise<ResponseResult>

  /**
   * Download file from url, with optional untar/unzip support.
   *
   * Note: you may need to set `strip` to 0 when using untar as extract method.
   *
   * @param {string} url
   * @param {DownloadOptions} options contains dest folder and optional onProgress callback
   */
  export function download(url: string, options: DownloadOptions, token?: CancellationToken): Promise<string>

  interface ExecOptions {
    cwd?: string
    env?: NodeJS.ProcessEnv
    shell?: string
    timeout?: number
    maxBuffer?: number
    killSignal?: string
    uid?: number
    gid?: number
    windowsHide?: boolean
  }

  /**
   * Dispose all disposables.
   */
  export function disposeAll(disposables: Disposable[]): void

  /**
   * Concurrent run async functions with limit support.
   */
  export function concurrent<T>(arr: T[], fn: (val: T) => Promise<void>, limit?: number): Promise<void>

  /**
   * Create promise resolved after ms milliseconds.
   */
  export function wait(ms: number): Promise<any>

  /**
   * Run command with `child_process.exec`
   */
  export function runCommand(cmd: string, opts?: ExecOptions, timeout?: number): Promise<string>

  /**
   * Check if process with pid is running
   */
  export function isRunning(pid: number): boolean

  /**
   * Check if command is executable.
   */
  export function executable(command: string): boolean

  /**
   * Watch single file for change, the filepath needs to be exists file.
   *
   * @param filepath Full path of file.
   * @param onChange Handler on file change detected.
   */
  export function watchFile(filepath: string, onChange: () => void): Disposable
  // }}

  // commands module {{
  export interface CommandItem {
    id: string
    internal?: boolean
    execute(...args: any[]): any
  }
  /**
   * Namespace for dealing with commands of coc.nvim
   */
  export namespace commands {
    /**
     * Registered commands.
     */
    export const commandList: CommandItem[]

    /**
     * Execute specified command.
     *
     * @deprecated use `executeCommand()` instead.
     */
    export function execute(command: { name: string, arguments?: any[] }): void

    /**
     * Check if command is registered.
     *
     * @param id Unique id of command.
     */
    export function has(id: string): boolean

    /**
     * Registers a command that can be invoked via a keyboard shortcut,
     * a menu item, an action, or directly.
     *
     * Registering a command with an existing command identifier twice
     * will cause an error.
     *
     * @param command A unique identifier for the command.
     * @param impl A command handler function.
     * @param thisArg The `this` context used when invoking the handler function.
     * @return Disposable which unregisters this command on disposal.
     */
    export function registerCommand(id: string, impl: (...args: any[]) => void, thisArg?: any, internal?: boolean): Disposable

    /**
     * Executes the command denoted by the given command identifier.
     *
     * * *Note 1:* When executing an editor command not all types are allowed to
     * be passed as arguments. Allowed are the primitive types `string`, `boolean`,
     * `number`, `undefined`, and `null`, as well as [`Position`](#Position), [`Range`](#Range), [`URI`](#URI) and [`Location`](#Location).
     * * *Note 2:* There are no restrictions when executing commands that have been contributed
     * by extensions.
     *
     * @param command Identifier of the command to execute.
     * @param rest Parameters passed to the command function.
     * @return A promise that resolves to the returned value of the given command. `undefined` when
     * the command handler function doesn't return anything.
     */
    export function executeCommand<T>(command: string, ...rest: any[]): Promise<T>

    /**
     * Open uri with external tool, use `open` on mac, use `xdg-open` on linux.
     */
    export function executeCommand(command: 'vscode.open', uri: string | Uri): Promise<void>

    /**
     * Reload current buffer by `:edit` command.
     */
    export function executeCommand(command: 'workbench.action.reloadWindow'): Promise<void>

    /**
     * Insert snippet at range of current buffer.
     *
     * @param edit Contains snippet text and range to replace.
     */
    export function executeCommand(command: 'editor.action.insertSnippet', edit: TextEdit, ultisnip?: UltiSnippetOption): Promise<boolean>

    /**
     * Invoke specified code action.
     */
    export function executeCommand(command: 'editor.action.doCodeAction', action: CodeAction): Promise<void>

    /**
     * Trigger coc.nvim's completion at current cursor position.
     */
    export function executeCommand(command: 'editor.action.triggerSuggest'): Promise<void>

    /**
     * Trigger signature help at current cursor position.
     */
    export function executeCommand(command: 'editor.action.triggerParameterHints'): Promise<void>

    /**
     * Add ranges to cursors session for multiple cursors.
     */
    export function executeCommand(command: 'editor.action.addRanges', ranges: Range[]): Promise<void>

    /**
     * Restart coc.nvim service by `:CocRestart` command.
     */
    export function executeCommand(command: 'editor.action.restart'): Promise<void>

    /**
     * Show locations by location list or vim's quickfix list.
     */
    export function executeCommand(command: 'editor.action.showReferences', filepath: string | undefined, position: Position | undefined, locations: Location[]): Promise<void>

    /**
     * Invoke rename action at position of specified uri.
     */
    export function executeCommand(command: 'editor.action.rename', uri: string, position: Position): Promise<void>

    /**
     * Run format action for current buffer.
     */
    export function executeCommand(command: 'editor.action.format'): Promise<void>
  }
  // }}

  // events module {{
  type EventResult = void | Promise<void>
  type MoveEvents = 'CursorMoved' | 'CursorMovedI' | 'CursorHold' | 'CursorHoldI'
  type BufEvents = 'BufHidden' | 'BufEnter' | 'BufWritePost'
    | 'InsertLeave' | 'TermOpen' | 'InsertEnter'
    | 'BufCreate' | 'BufUnload' | 'BufWritePre' | 'Enter'
  type EmptyEvents = 'FocusGained' | 'FocusLost' | 'InsertSnippet'
  type InsertChangeEvents = 'TextChangedP' | 'TextChangedI'
  type TaskEvents = 'TaskExit' | 'TaskStderr' | 'TaskStdout'
  type WindowEvents = 'WinLeave' | 'WinEnter'
  type AllEvents = BufEvents | EmptyEvents | MoveEvents | TaskEvents | WindowEvents | InsertChangeEvents | 'CompleteDone' | 'TextChanged' | 'MenuPopupChanged' | 'InsertCharPre' | 'FileType' | 'BufWinEnter' | 'BufWinLeave' | 'VimResized' | 'DirChanged' | 'OptionSet' | 'Command' | 'BufReadCmd' | 'GlobalChange' | 'InputChar' | 'WinLeave' | 'MenuInput' | 'PromptInsert' | 'FloatBtnClick' | 'InsertSnippet' | 'PromptKeyPress'
  type OptionValue = string | number | boolean
  type PromptWidowKeys = 'C-j' | 'C-k' | 'C-n' | 'C-p' | 'up' | 'down'

  export interface CursorPosition {
    readonly bufnr: number
    readonly lnum: number
    readonly col: number
    readonly insert: boolean
  }

  export interface InsertChange {
    /**
     * 1 based line number
     */
    readonly lnum: number
    /**
     * 1 based column number
     */
    readonly col: number
    /**
     * Text before cursor.
     */
    readonly pre: string
    /**
     * Insert character that cause change of this time.
     */
    readonly insertChar: string | undefined
    readonly changedtick: number
  }

  export interface PopupChangeEvent {
    readonly completed_item: VimCompleteItem
    readonly height: number
    readonly width: number
    readonly row: number
    readonly col: number
    readonly size: number
    readonly scrollbar: boolean
  }

  /**
   * Used for listen to events send from vim.
   */
  export namespace events {
    /**
     * Latest cursor position.
     */
    export const cursor: Readonly<CursorPosition>
    /**
     * Latest pum position, is true when pum positioned above current line.
     */
    export const pumAlignTop: boolean
    /**
     * Insert mode detected by latest events.
     */
    export const insertMode: boolean

    /**
     * Popup menu is visible.
     */
    export const pumvisible: boolean

    /**
     * Wait for any of event in events to fire, resolve undefined when timeout or CancellationToken requested.
     * @param events Event names to wait.
     * @param timeoutOrToken Timeout in miniseconds or CancellationToken.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function race(events: AllEvents[], timeoutOrToken?: number | CancellationToken): Promise<{ name: AllEvents, args: unknown[] } | undefined>

    /**
     * Attach handler to buffer events.
     */
    export function on(event: BufEvents, handler: (bufnr: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    /**
     * Attach handler to mouse move events.
     */
    export function on(event: MoveEvents, handler: (bufnr: number, cursor: [number, number]) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    /**
     * Attach handler to TextChangedI or TextChangedP.
     */
    export function on(event: InsertChangeEvents, handler: (bufnr: number, info: InsertChange) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    /**
     * Attach handler to window event.
     */
    export function on(event: WindowEvents, handler: (winid: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    /**
     * Attach handler to float button click.
     */
    export function on(event: 'FloatBtnClick', handler: (bufnr: number, index: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    /**
     * Attach handler to keypress in prompt window.
     * Key could only be 'C-j', 'C-k', 'C-n', 'C-p', 'up' and 'down'
     */
    export function on(event: 'PromptKeyPress', handler: (bufnr: number, key: PromptWidowKeys) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    /**
     * Fired on vim's TextChanged event.
     */
    export function on(event: 'TextChanged', handler: (bufnr: number, changedtick: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'TaskExit', handler: (id: string, code: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'TaskStderr' | 'TaskStdout', handler: (id: string, lines: string[]) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    /**
     * Fired on vim's BufReadCmd event.
     */
    export function on(event: 'BufReadCmd', handler: (scheme: string, fullpath: string) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    /**
     * Fired on vim's VimResized event.
     */
    export function on(event: 'VimResized', handler: (columns: number, lines: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'MenuPopupChanged', handler: (event: PopupChangeEvent, cursorline: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'CompleteDone', handler: (item: VimCompleteItem) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'InsertCharPre', handler: (character: string) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'FileType', handler: (filetype: string, bufnr: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'BufWinEnter' | 'BufWinLeave', handler: (bufnr: number, winid: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'DirChanged', handler: (cwd: string) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'OptionSet' | 'GlobalChange', handler: (option: string, oldVal: OptionValue, newVal: OptionValue) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'InputChar', handler: (session: string, character: string, mode: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'PromptInsert', handler: (value: string, bufnr: number) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
    export function on(event: 'Command', handler: (name: string) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable

    /**
     * Fired after user insert character and made change to the buffer.
     * Fired after TextChangedI & TextChangedP event.
     */
    export function on(event: 'TextInsert', handler: (bufnr: number, info: InsertChange, character: string) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable

    export function on(event: EmptyEvents, handler: () => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable

    export function on(event: AllEvents[], handler: (...args: unknown[]) => EventResult, thisArg?: any, disposables?: Disposable[]): Disposable
  }
  // }}

  // file events {{
  /**
   * An event that is fired after files are created.
   */
  export interface FileCreateEvent {

    /**
     * The files that got created.
     */
    readonly files: ReadonlyArray<Uri>
  }

  /**
   * An event that is fired when files are going to be created.
   *
   * To make modifications to the workspace before the files are created,
   * call the [`waitUntil](#FileWillCreateEvent.waitUntil)-function with a
   * thenable that resolves to a [workspace edit](#WorkspaceEdit).
   */
  export interface FileWillCreateEvent {

    /**
    * A cancellation token.
    */
    readonly token: CancellationToken

    /**
     * The files that are going to be created.
     */
    readonly files: ReadonlyArray<Uri>

    /**
     * Allows to pause the event and to apply a [workspace edit](#WorkspaceEdit).
     *
     * *Note:* This function can only be called during event dispatch and not
     * in an asynchronous manner:
     *
     * ```ts
     * workspace.onWillCreateFiles(event => {
     *     // async, will *throw* an error
     *     setTimeout(() => event.waitUntil(promise));
     *
     *     // sync, OK
     *     event.waitUntil(promise);
     * })
     * ```
     *
     * @param thenable A thenable that delays saving.
     */
    waitUntil(thenable: Thenable<WorkspaceEdit | any>): void
  }

  /**
   * An event that is fired when files are going to be deleted.
   *
   * To make modifications to the workspace before the files are deleted,
   * call the [`waitUntil](#FileWillCreateEvent.waitUntil)-function with a
   * thenable that resolves to a [workspace edit](#WorkspaceEdit).
   */
  export interface FileWillDeleteEvent {

    /**
     * The files that are going to be deleted.
     */
    readonly files: ReadonlyArray<Uri>

    /**
     * Allows to pause the event and to apply a [workspace edit](#WorkspaceEdit).
     *
     * *Note:* This function can only be called during event dispatch and not
     * in an asynchronous manner:
     *
     * ```ts
     * workspace.onWillCreateFiles(event => {
     *     // async, will *throw* an error
     *     setTimeout(() => event.waitUntil(promise));
     *
     *     // sync, OK
     *     event.waitUntil(promise);
     * })
     * ```
     *
     * @param thenable A thenable that delays saving.
     */
    waitUntil(thenable: Thenable<WorkspaceEdit | any>): void
  }

  /**
   * An event that is fired after files are deleted.
   */
  export interface FileDeleteEvent {

    /**
     * The files that got deleted.
     */
    readonly files: ReadonlyArray<Uri>
  }

  /**
   * An event that is fired after files are renamed.
   */
  export interface FileRenameEvent {

    /**
     * The files that got renamed.
     */
    readonly files: ReadonlyArray<{ oldUri: Uri, newUri: Uri }>
  }

  /**
   * An event that is fired when files are going to be renamed.
   *
   * To make modifications to the workspace before the files are renamed,
   * call the [`waitUntil](#FileWillCreateEvent.waitUntil)-function with a
   * thenable that resolves to a [workspace edit](#WorkspaceEdit).
   */
  export interface FileWillRenameEvent {

    /**
     * The files that are going to be renamed.
     */
    readonly files: ReadonlyArray<{ oldUri: Uri, newUri: Uri }>

    /**
     * Allows to pause the event and to apply a [workspace edit](#WorkspaceEdit).
     *
     * *Note:* This function can only be called during event dispatch and not
     * in an asynchronous manner:
     *
     * ```ts
     * workspace.onWillCreateFiles(event => {
     * 	// async, will *throw* an error
     * 	setTimeout(() => event.waitUntil(promise));
     *
     * 	// sync, OK
     * 	event.waitUntil(promise);
     * })
     * ```
     *
     * @param thenable A thenable that delays saving.
     */
    waitUntil(thenable: Thenable<WorkspaceEdit | any>): void
  }
  // }}

  // languages module {{
  export interface DocumentSymbolProviderMetadata {
    /**
    * A human-readable string that is shown when multiple outlines trees show for one document.
    */
    label?: string
  }

  export namespace languages {
    /**
     * Create a diagnostics collection.
     *
     * @param name The [name](#DiagnosticCollection.name) of the collection.
     * @return A new diagnostic collection.
     */
    export function createDiagnosticCollection(name?: string): DiagnosticCollection

    /**
     * Register a formatting provider that works on type. The provider is active when the user enables the setting `coc.preferences.formatOnType`.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their [score](#languages.match) and the best-matching provider is used. Failure
     * of the selected provider will cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider An on type formatting edit provider.
     * @param triggerCharacters Trigger character that should trigger format on type.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerOnTypeFormattingEditProvider(selector: DocumentSelector, provider: OnTypeFormattingEditProvider, triggerCharacters: string[]): Disposable

    /**
     * Register a completion provider.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their [score](#languages.match) and groups of equal score are sequentially asked for
     * completion items. The process stops when one or many providers of a group return a
     * result. A failing provider (rejected promise or exception) will not fail the whole
     * operation.
     *
     * A completion item provider can be associated with a set of `triggerCharacters`. When trigger
     * characters are being typed, completions are requested but only from providers that registered
     * the typed character. Because of that trigger characters should be different than [word characters](#LanguageConfiguration.wordPattern),
     * a common trigger character is `.` to trigger member completions.
     *
     * @param name Name of completion source.
     * @param shortcut Shortcut used in completion menu.
     * @param selector Document selector of created completion source.
     * @param provider A completion provider.
     * @param triggerCharacters Trigger completion when the user types one of the characters.
     * @param priority Higher priority would shown first.
     * @param allCommitCharacters Commit characters of completion source.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerCompletionItemProvider(name: string, shortcut: string, selector: DocumentSelector | null, provider: CompletionItemProvider, triggerCharacters?: string[], priority?: number, allCommitCharacters?: string[]): Disposable

    /**
     * Register a code action provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A code action provider.
     * @param clientId Optional id of language client.
     * @param codeActionKinds Optional supported code action kinds.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerCodeActionProvider(selector: DocumentSelector, provider: CodeActionProvider, clientId: string | undefined, codeActionKinds?: string[]): Disposable

    /**
     * Register a hover provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A hover provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerHoverProvider(selector: DocumentSelector, provider: HoverProvider): Disposable

    /**
     * Register a selection range provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A selection range provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerSelectionRangeProvider(selector: DocumentSelector, provider: SelectionRangeProvider): Disposable

    /**
     * Register a signature help provider.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their [score](#languages.match) and called sequentially until a provider returns a
     * valid result.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A signature help provider.
     * @param triggerCharacters Trigger signature help when the user types one of the characters, like `,` or `(`.
     * @param metadata Information about the provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerSignatureHelpProvider(selector: DocumentSelector, provider: SignatureHelpProvider, triggerCharacters?: string[]): Disposable

    /**
     * Register a document symbol provider.
     *
     * Multiple providers can be registered for a language. In that case providers only first provider
     * are asked for result.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A document symbol provider.
     * @param metadata Optional meta data.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerDocumentSymbolProvider(selector: DocumentSelector, provider: DocumentSymbolProvider, metadata?: DocumentSymbolProviderMetadata): Disposable

    /**
     * Register a folding range provider.
     *
     * Multiple providers can be registered for a language. In that case providers only first provider
     * are asked for result.
     *
     * A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A folding range provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerFoldingRangeProvider(selector: DocumentSelector, provider: FoldingRangeProvider): Disposable

    /**
     * Register a document highlight provider.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their [score](#languages.match) and groups sequentially asked for document highlights.
     * The process stops when a provider returns a `non-falsy` or `non-failure` result.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A document highlight provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerDocumentHighlightProvider(selector: DocumentSelector, provider: any): Disposable

    /**
     * Register a code lens provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A code lens provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerCodeLensProvider(selector: DocumentSelector, provider: CodeLensProvider): Disposable

    /**
     * Register a document link provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A document link provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerDocumentLinkProvider(selector: DocumentSelector, provider: DocumentLinkProvider): Disposable

    /**
     * Register a color provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A color provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerDocumentColorProvider(selector: DocumentSelector, provider: DocumentColorProvider): Disposable

    /**
     * Register a definition provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A definition provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerDefinitionProvider(selector: DocumentSelector, provider: DefinitionProvider): Disposable

    /**
     * Register a declaration provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A declaration provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerDeclarationProvider(selector: DocumentSelector, provider: DeclarationProvider): Disposable


    /**
     * Register a type definition provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A type definition provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerTypeDefinitionProvider(selector: DocumentSelector, provider: TypeDefinitionProvider): Disposable

    /**
     * Register an implementation provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider An implementation provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerImplementationProvider(selector: DocumentSelector, provider: ImplementationProvider): Disposable

    /**
     * Register a reference provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A reference provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerReferencesProvider(selector: DocumentSelector, provider: ReferenceProvider): Disposable

    /**
     * Register a rename provider.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their [score](#workspace.match) and asked in sequence. The first provider producing a result
     * defines the result of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A rename provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerRenameProvider(selector: DocumentSelector, provider: RenameProvider): Disposable

    /**
     * Register a workspace symbol provider.
     *
     * Multiple providers can be registered. In that case providers are asked in parallel and
     * the results are merged. A failing provider (rejected promise or exception) will not cause
     * a failure of the whole operation.
     *
     * @param provider A workspace symbol provider.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerWorkspaceSymbolProvider(provider: WorkspaceSymbolProvider): Disposable

    /**
     * Register a formatting provider for a document.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their priority. Failure of the selected provider will cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A document formatting edit provider.
     * @param priority default to 0.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerDocumentFormatProvider(selector: DocumentSelector, provider: DocumentFormattingEditProvider, priority?: number): Disposable

    /**
     * Register a formatting provider for a document range.
     *
     * *Note:* A document range provider is also a [document formatter](#DocumentFormattingEditProvider)
     * which means there is no need to [register](#languages.registerDocumentFormattingEditProvider) a document
     * formatter when also registering a range provider.
     *
     * Multiple providers can be registered for a language. In that case provider with highest priority is used.
     * Failure of the selected provider will cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A document range formatting edit provider.
     * @param priority default to 0.
     * @return A [disposable](#Disposable) that unregisters this provider when being disposed.
     */
    export function registerDocumentRangeFormatProvider(selector: DocumentSelector, provider: DocumentRangeFormattingEditProvider, priority?: number): Disposable

    /**
     * Register a call hierarchy provider.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A call hierarchy provider.
     * @return A {@link Disposable} that unregisters this provider when being disposed.
     */
    export function registerCallHierarchyProvider(selector: DocumentSelector, provider: CallHierarchyProvider): Disposable

    /**
     * Register a semantic tokens provider for a whole document.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their {@link languages.match score} and the best-matching provider is used. Failure
     * of the selected provider will cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A document semantic tokens provider.
     * @return A {@link Disposable} that unregisters this provider when being disposed.
     */
    export function registerDocumentSemanticTokensProvider(selector: DocumentSelector, provider: DocumentSemanticTokensProvider, legend: SemanticTokensLegend): Disposable

    /**
     * Register a semantic tokens provider for a document range.
     *
     * *Note:* If a document has both a `DocumentSemanticTokensProvider` and a `DocumentRangeSemanticTokensProvider`,
     * the range provider will be invoked only initially, for the time in which the full document provider takes
     * to resolve the first request. Once the full document provider resolves the first request, the semantic tokens
     * provided via the range provider will be discarded and from that point forward, only the document provider
     * will be used.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their {@link languages.match score} and the best-matching provider is used. Failure
     * of the selected provider will cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A document range semantic tokens provider.
     * @return A {@link Disposable} that unregisters this provider when being disposed.
     */
    export function registerDocumentRangeSemanticTokensProvider(selector: DocumentSelector, provider: DocumentRangeSemanticTokensProvider, legend: SemanticTokensLegend): Disposable

    /**
     * Register a linked editing range provider.
     *
     * Multiple providers can be registered for a language. In that case providers are sorted
     * by their {@link languages.match score} and the best-matching provider that has a result is used. Failure
     * of the selected provider will cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider A linked editing range provider.
     * @return A {@link Disposable} that unregisters this provider when being disposed.
     */
    export function registerLinkedEditingRangeProvider(selector: DocumentSelector, provider: LinkedEditingRangeProvider): Disposable

    /**
     * Register a inlay hints provider.
     *
     * Multiple providers can be registered for a language. In that case providers are asked in
     * parallel and the results are merged. A failing provider (rejected promise or exception) will
     * not cause a failure of the whole operation.
     *
     * @param selector A selector that defines the documents this provider is applicable to.
     * @param provider An inlay hints provider.
     * @return A {@link Disposable} that unregisters this provider when being disposed.
     */
    export function registerInlayHintsProvider(selector: DocumentSelector, provider: InlayHintsProvider): Disposable
  }
  // }}

  // services module {{
  export enum ServiceStat {
    Initial,
    Starting,
    StartFailed,
    Running,
    Stopping,
    Stopped,
  }

  export interface IServiceProvider {
    // unique service id
    id: string
    name: string
    client?: LanguageClient
    selector: DocumentSelector
    // current state
    state: ServiceStat
    start(): Promise<void>
    dispose(): void
    stop(): Promise<void> | void
    restart(): Promise<void> | void
    onServiceReady: Event<void>
  }

  export namespace services {
    /**
     * Register languageClient as service provider.
     */
    export function registLanguageClient(client: LanguageClient): Disposable
    /**
     * Register service, nothing happens when `service.id` already exists.
     */
    export function regist(service: IServiceProvider): Disposable
    /**
     * Get service by id.
     */
    export function getService(id: string): IServiceProvider
    /**
     * Stop service by id.
     */
    export function stop(id: string): Promise<void>
    /**
     * Stop running service or start stopped service.
     */
    export function toggle(id: string): Promise<void>
  }
  // }}

  // sources module {{
  /**
   * Source options to create source that could respect configuration from `coc.source.{name}`
   */
  export type SourceConfig = Omit<ISource, 'shortcut' | 'priority' | 'triggerOnly' | 'triggerCharacters' | 'triggerPatterns' | 'enable' | 'filetypes' | 'disableSyntaxes'>

  export interface SourceStat {
    name: string
    priority: number
    triggerCharacters: string[]
    type: 'native' | 'remote' | 'service'
    shortcut: string
    filepath: string
    disabled: boolean
    filetypes: string[]
  }

  export enum SourceType {
    Native,
    Remote,
    Service,
  }

  export interface CompleteResult {
    items: VimCompleteItem[]
    isIncomplete?: boolean
    startcol?: number
    source?: string
    priority?: number
  }

  // option on complete & should_complete
  export interface CompleteOption {
    /**
     * Current buffer number.
     */
    readonly bufnr: number
    /**
     * Current line.
     */
    readonly line: string
    /**
     * Column to start completion, determined by iskeyword options of buffer.
     */
    readonly col: number
    /**
     * Input text.
     */
    readonly input: string
    readonly filetype: string
    readonly filepath: string
    /**
     * Word under cursor.
     */
    readonly word: string
    /**
     * Trigger character, could be empty string.
     */
    readonly triggerCharacter: string
    /**
     * Col of cursor, 1 based.
     */
    readonly colnr: number
    readonly linenr: number
    readonly synname: string
    /**
     * Black list words specified by user.
     */
    readonly blacklist: string[]
    /**
     * Buffer changetick
     */
    readonly changedtick: number
    /**
     * Is trigger for in complete completion.
     */
    readonly triggerForInComplete?: boolean
  }

  export interface ISource {
    /**
     * Identifier name
     */
    name: string
    /**
     * @deprecated use documentSelector instead.
     */
    filetypes?: string[]
    /**
     * Filters of document.
     */
    documentSelector?: DocumentSelector
    enable?: boolean
    shortcut?: string
    priority?: number
    sourceType?: SourceType
    /**
     * Should only be used when completion is triggered, requirs `triggerPatterns` or `triggerCharacters` defined.
     */
    triggerOnly?: boolean
    triggerCharacters?: string[]
    // regex to detect trigger completion, ignored when triggerCharacters exists.
    triggerPatterns?: RegExp[]
    disableSyntaxes?: string[]
    filepath?: string
    // should the first character always match
    firstMatch?: boolean
    refresh?(): Promise<void>
    /**
     * For disable/enable
     */
    toggle?(): void

    /**
     * Triggered on BufEnter, used for cache normally
     */
    onEnter?(bufnr: number): void

    /**
     * Check if this source should doComplete
     *
     * @public
     * @param {CompleteOption} opt
     * @returns {Promise<boolean> }
     */
    shouldComplete?(opt: CompleteOption): Promise<boolean>

    /**
     * Run completion
     *
     * @public
     * @param {CompleteOption} opt
     * @param {CancellationToken} token
     * @returns {Promise<CompleteResult | null>}
     */
    doComplete(opt: CompleteOption, token: CancellationToken): ProviderResult<CompleteResult>

    /**
     * Action for complete item on complete item selected
     *
     * @public
     * @param {VimCompleteItem} item
     * @param {CancellationToken} token
     * @returns {Promise<void>}
     */
    onCompleteResolve?(item: VimCompleteItem, token: CancellationToken): ProviderResult<void>

    /**
     * Action for complete item on complete done
     *
     * @public
     * @param {VimCompleteItem} item
     * @returns {Promise<void>}
     */
    onCompleteDone?(item: VimCompleteItem, opt: CompleteOption): ProviderResult<void>

    shouldCommit?(item: VimCompleteItem, character: string): boolean
  }

  export namespace sources {
    /**
     * Names of registered sources.
     */
    export const names: ReadonlyArray<string>
    export const sources: ReadonlyArray<ISource>
    /**
     * Check if source exists by name.
     */
    export function has(name: string): boolean
    /**
     * Get source by name.
     */
    export function getSource(name: string): ISource | null

    /**
     * Add source to sources list.
     *
     * Note: Use `sources.createSource()` for regist new source is recommended for
     * user configuration support.
     */
    export function addSource(source: ISource): Disposable

    /**
     * Create source by source config, configurations starts with `coc.source.{name}`
     * are automatically supported.
     *
     * `name` and `doComplete()` must be provided in config.
     */
    export function createSource(config: SourceConfig): Disposable

    /**
     * Get list of all source stats.
     */
    export function sourceStats(): SourceStat[]

    /**
     * Call refresh for _name_ source or all sources.
     */
    export function refresh(name?: string): Promise<void>

    /**
     * Toggle state of _name_ source.
     */
    export function toggleSource(name: string): void

    /**
     * Remove source by name.
     */
    export function removeSource(name: string): void
  }
  // }}

  // TreeView related {{
  export interface TreeItemLabel {
    label: string
    highlights?: [number, number][]
  }

  export interface TreeItemIcon {
    text: string
    hlGroup: string
  }

  /**
   * Collapsible state of the tree item
   */
  export enum TreeItemCollapsibleState {
    /**
     * Determines an item can be neither collapsed nor expanded. Implies it has no children.
     */
    None = 0,
    /**
     * Determines an item is collapsed
     */
    Collapsed = 1,
    /**
     * Determines an item is expanded
     */
    Expanded = 2
  }

  export class TreeItem {
    /**
     * A human-readable string describing this item. When `falsy`, it is derived from {@link TreeItem.resourceUri resourceUri}.
     */
    label?: string | TreeItemLabel

    /**
     * Description rendered less prominently after label.
     */
    description?: string

    /**
     * The icon path or {@link ThemeIcon} for the tree item.
     * When `falsy`, {@link ThemeIcon.Folder Folder Theme Icon} is assigned, if item is collapsible otherwise {@link ThemeIcon.File File Theme Icon}.
     * When a file or folder {@link ThemeIcon} is specified, icon is derived from the current file icon theme for the specified theme icon using {@link TreeItem.resourceUri resourceUri} (if provided).
     */
    icon?: TreeItemIcon

    /**
     * Optional id for the tree item that has to be unique across tree. The id is used to preserve the selection and expansion state of the tree item.
     *
     * If not provided, an id is generated using the tree item's resourceUri when exists. **Note** that when labels change, ids will change and that selection and expansion state cannot be kept stable anymore.
     */
    id?: string

    /**
     * The {@link Uri} of the resource representing this item.
     *
     * Will be used to derive the {@link TreeItem.label label}, when it is not provided.
     * Will be used to derive the icon from current file icon theme, when {@link TreeItem.iconPath iconPath} has {@link ThemeIcon} value.
     */
    resourceUri?: Uri

    /**
     * The tooltip text when you hover over this item.
     */
    tooltip?: string | MarkupContent

    /**
     * The {@link Command} that should be executed when the tree item is selected.
     *
     * Please use `vscode.open` or `vscode.diff` as command IDs when the tree item is opening
     * something in the editor. Using these commands ensures that the resulting editor will
     * appear consistent with how other built-in trees open editors.
     */
    command?: Command

    /**
     * {@link TreeItemCollapsibleState} of the tree item.
     */
    collapsibleState?: TreeItemCollapsibleState

    /**
     * @param label A human-readable string describing this item
     * @param collapsibleState {@link TreeItemCollapsibleState} of the tree item. Default is {@link TreeItemCollapsibleState.None}
     */
    constructor(label: string | TreeItemLabel, collapsibleState?: TreeItemCollapsibleState)

    /**
     * @param resourceUri The {@link Uri} of the resource representing this item.
     * @param collapsibleState {@link TreeItemCollapsibleState} of the tree item. Default is {@link TreeItemCollapsibleState.None}
     */
    constructor(resourceUri: Uri, collapsibleState?: TreeItemCollapsibleState)
  }

  /**
   * Action resolved by {@link TreeDataProvider}
   */
  export interface TreeItemAction<T> {
    /**
     * Label text in menu.
     */
    title: string
    handler: (item: T) => ProviderResult<void>
  }

  /**
   * Options for creating a {@link TreeView}
   */
  export interface TreeViewOptions<T> {
    /**
     * bufhidden option for TreeView, default to 'wipe'
     */
    bufhidden?: 'hide' | 'unload' | 'delete' | 'wipe'
    /**
     * Fixed width for window, default to true
     */
    winfixwidth?: boolean
    /**
     * Enable filter feature, default to false
     */
    enableFilter?: boolean
    /**
     * Disable indent of leaves without children, default to false
     */
    disableLeafIndent?: boolean
    /**
     * A data provider that provides tree data.
     */
    treeDataProvider: TreeDataProvider<T>
    /**
     * Whether the tree supports multi-select. When the tree supports multi-select and a command is executed from the tree,
     * the first argument to the command is the tree item that the command was executed on and the second argument is an
     * array containing all selected tree items.
     */
    canSelectMany?: boolean
  }

  /**
   * The event that is fired when an element in the {@link TreeView} is expanded or collapsed
   */
  export interface TreeViewExpansionEvent<T> {

    /**
     * Element that is expanded or collapsed.
     */
    readonly element: T

  }

  /**
   * The event that is fired when there is a change in {@link TreeView.selection tree view's selection}
   */
  export interface TreeViewSelectionChangeEvent<T> {

    /**
     * Selected elements.
     */
    readonly selection: T[]

  }

  /**
   * The event that is fired when there is a change in {@link TreeView.visible tree view's visibility}
   */
  export interface TreeViewVisibilityChangeEvent {

    /**
     * `true` if the {@link TreeView tree view} is visible otherwise `false`.
     */
    readonly visible: boolean

  }

  /**
   * Represents a Tree view
   */
  export interface TreeView<T> extends Disposable {

    /**
     * Event that is fired when an element is expanded
     */
    readonly onDidExpandElement: Event<TreeViewExpansionEvent<T>>

    /**
     * Event that is fired when an element is collapsed
     */
    readonly onDidCollapseElement: Event<TreeViewExpansionEvent<T>>

    /**
     * Currently selected elements.
     */
    readonly selection: T[]

    /**
     * Event that is fired when the {@link TreeView.selection selection} has changed
     */
    readonly onDidChangeSelection: Event<TreeViewSelectionChangeEvent<T>>

    /**
     * Event that is fired when {@link TreeView.visible visibility} has changed
     */
    readonly onDidChangeVisibility: Event<TreeViewVisibilityChangeEvent>

    /**
     * `true` if the {@link TreeView tree view} is visible otherwise `false`.
     *
     * **NOTE:** is `true` when TreeView visible on other tab.
     */
    readonly visible: boolean

    /**
     * Window id used by TreeView.
     */
    readonly windowId: number | undefined

    /**
     * An optional human-readable message that will be rendered in the view.
     * Setting the message to null, undefined, or empty string will remove the message from the view.
     */
    message?: string

    /**
     * The tree view title is initially taken from viewId of TreeView
     * Changes to the title property will be properly reflected in the UI in the title of the view.
     */
    title?: string

    /**
     * An optional human-readable description which is rendered less prominently in the title of the view.
     * Setting the title description to null, undefined, or empty string will remove the description from the view.
     */
    description?: string

    /**
     * Reveals the given element in the tree view.
     * If the tree view is not visible then the tree view is shown and element is revealed.
     *
     * By default revealed element is selected.
     * In order to not to select, set the option `select` to `false`.
     * In order to focus, set the option `focus` to `true`.
     * In order to expand the revealed element, set the option `expand` to `true`. To expand recursively set `expand` to the number of levels to expand.
     * **NOTE:** You can expand only to 3 levels maximum.
     *
     * **NOTE:** The {@link TreeDataProvider} that the `TreeView` {@link window.createTreeView is registered with} with must implement {@link TreeDataProvider.getParent getParent} method to access this API.
     */
    reveal(element: T, options?: { select?: boolean, focus?: boolean, expand?: boolean | number }): Thenable<void>

    /**
     * Create tree view in new window.
     *
     * **NOTE:** TreeView with same viewId in current tab would be disposed.
     *
     * @param splitCommand The command to open TreeView window, default to 'belowright 30vs'
     * @return `true` if window shown.
     */
    show(splitCommand?: string): Promise<boolean>
  }

  /**
   * A data provider that provides tree data
   */
  export interface TreeDataProvider<T> {
    /**
     * An optional event to signal that an element or root has changed.
     * This will trigger the view to update the changed element/root and its children recursively (if shown).
     * To signal that root has changed, do not pass any argument or pass `undefined` or `null`.
     */
    onDidChangeTreeData?: Event<T | undefined | null | void>

    /**
     * Get {@link TreeItem} representation of the `element`
     *
     * @param element The element for which {@link TreeItem} representation is asked for.
     * @return {@link TreeItem} representation of the element
     */
    getTreeItem(element: T): TreeItem | Thenable<TreeItem>

    /**
     * Get the children of `element` or root if no element is passed.
     *
     * @param element The element from which the provider gets children. Can be `undefined`.
     * @return Children of `element` or root if no element is passed.
     */
    getChildren(element?: T): ProviderResult<T[]>

    /**
     * Optional method to return the parent of `element`.
     * Return `null` or `undefined` if `element` is a child of root.
     *
     * **NOTE:** This method should be implemented in order to access {@link TreeView.reveal reveal} API.
     *
     * @param element The element for which the parent has to be returned.
     * @return Parent of `element`.
     */
    getParent?(element: T): ProviderResult<T>

    /**
     * Called on hover to resolve the {@link TreeItem.tooltip TreeItem} property if it is undefined.
     * Called on tree item click/open to resolve the {@link TreeItem.command TreeItem} property if it is undefined.
     * Only properties that were undefined can be resolved in `resolveTreeItem`.
     * Functionality may be expanded later to include being called to resolve other missing
     * properties on selection and/or on open.
     *
     * Will only ever be called once per TreeItem.
     *
     * onDidChangeTreeData should not be triggered from within resolveTreeItem.
     *
     * *Note* that this function is called when tree items are already showing in the UI.
     * Because of that, no property that changes the presentation (label, description, etc.)
     * can be changed.
     *
     * @param item Undefined properties of `item` should be set then `item` should be returned.
     * @param element The object associated with the TreeItem.
     * @param token A cancellation token.
     * @return The resolved tree item or a thenable that resolves to such. It is OK to return the given
     * `item`. When no result is returned, the given `item` will be used.
     */
    resolveTreeItem?(item: TreeItem, element: T, token: CancellationToken): ProviderResult<TreeItem>

    /**
     * Called with current element to resolve actions.
     * Called when user press 'actions' key.
     *
     * @param item Resolved item.
     * @param element The object under cursor.
     */
    resolveActions?(item: TreeItem, element: T): ProviderResult<TreeItemAction<T>[]>
  }
  // }}

  // workspace module {{
  /**
   * An event describing the change in Configuration
   */
  export interface ConfigurationChangeEvent {

    /**
     * Returns `true` if the given section for the given resource (if provided) is affected.
     *
     * @param section Configuration name, supports _dotted_ names.
     * @param resource A resource URI.
     * @return `true` if the given section for the given resource (if provided) is affected.
     */
    affectsConfiguration(section: string, resource?: string): boolean
  }

  export interface WillSaveEvent extends TextDocumentWillSaveEvent {
    /**
     * Allows to pause the event loop and to apply [pre-save-edits](#TextEdit).
     * Edits of subsequent calls to this function will be applied in order. The
     * edits will be *ignored* if concurrent modifications of the document happened.
     *
     * *Note:* This function can only be called during event dispatch and not
     * in an asynchronous manner:
     *
     * ```ts
     * workspace.onWillSaveTextDocument(event => {
     * 	// async, will *throw* an error
     * 	setTimeout(() => event.waitUntil(promise));
     *
     * 	// sync, OK
     * 	event.waitUntil(promise);
     * })
     * ```
     *
     * @param thenable A thenable that resolves to [pre-save-edits](#TextEdit).
     */
    waitUntil(thenable: Thenable<TextEdit[] | any>): void
  }

  export interface KeymapOption {
    /**
     * Use request instead of notify, default true
     */
    sync: boolean
    /**
     * Cancel completion before invoke callback, default true
     */
    cancel: boolean
    /**
     * Use <silent> for keymap, default false
     */
    silent: boolean
    /**
     * Enable repeat support for repeat.vim, default false
     */
    repeat: boolean
  }

  export interface DidChangeTextDocumentParams {
    /**
     * The document that did change. The version number points
     * to the version after all provided content changes have
     * been applied.
     */
    readonly textDocument: {
      version: number
      uri: string
    }
    /**
     * The actual content changes. The content changes describe single state changes
     * to the document. So if there are two content changes c1 (at array index 0) and
     * c2 (at array index 1) for a document in state S then c1 moves the document from
     * S to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed
     * on the state S'.
     */
    readonly contentChanges: ReadonlyArray<TextDocumentContentChange>
    /**
     * Buffer number of document.
     */
    readonly bufnr: number
    /**
     * Original content before change
     */
    readonly original: string
    /**
     * Original lines before change
     */
    readonly originalLines: ReadonlyArray<string>
  }

  export interface EditerState {
    document: LinesTextDocument
    position: Position
  }

  export type MapMode = 'n' | 'i' | 'v' | 'x' | 's' | 'o'

  export interface Autocmd {
    /**
     * Vim event or event set.
     */
    event: string | string[]
    /**
     * Callback functions that called with evaled arguments.
     */
    callback: Function
    /**
     * Match pattern, default to `*`.
     */
    pattern?: string
    /**
     * Vim expression that eval to arguments of callback, default to `[]`
     */
    arglist?: string[]
    /**
     * Use request when `true`, use notification by default.
     */
    request?: boolean
    /**
     * `this` of callback.
     */
    thisArg?: any
  }

  export interface Env {
    /**
     * |completeopt| option of (neo)vim.
     */
    readonly completeOpt: string
    /**
     * |runtimepath| option of (neo)vim.
     */
    readonly runtimepath: string
    /**
     * |guicursor| option of (neo)vim
     */
    readonly guicursor: string
    /**
     * Could use float window on neovim, always false on vim.
     */
    readonly floating: boolean
    /**
     * |sign_place()| and |sign_unplace()| can be used when true.
     */
    readonly sign: boolean
    /**
     * Root directory of extensions.
     */
    readonly extensionRoot: string
    /**
     * Process id of (neo)vim.
     */
    readonly pid: number
    /**
     * Total columns of screen.
     */
    readonly columns: number
    /**
     * Total lines of screen.
     */
    readonly lines: number
    /**
     * Is true when |CompleteChanged| event is supported.
     */
    readonly pumevent: boolean
    /**
     * |cmdheight| option of (neo)vim.
     */
    readonly cmdheight: number
    /**
     * Value of |g:coc_filetype_map|
     */
    readonly filetypeMap: { [index: string]: string }
    /**
     * Is true when not using neovim.
     */
    readonly isVim: boolean
    /**
     * Is cygvim when true.
     */
    readonly isCygwin: boolean
    /**
     * Is macvim when true.
     */
    readonly isMacvim: boolean
    /**
     * Is true when iTerm.app is used on mac.
     */
    readonly isiTerm: boolean
    /**
     * version of (neo)vim, on vim it's like: 8020750, on neoivm it's like: 0.5.0
     */
    readonly version: string
    /**
     * |v:progpath| value, could be empty.
     */
    readonly progpath: string
    /**
     * Is true when dialog feature is supported, which need vim >= 8.2.750 or neovim >= 0.4.0
     */
    readonly dialog: boolean
    /**
     * Is true when vim's textprop is supported.
     */
    readonly textprop: boolean
  }

  /**
   * Store & retrieve most recent used items.
   */
  export interface Mru {
    /**
     * Load iems from mru file
     */

    load(): Promise<string[]>
    /**
     * Add item to mru file.
     */
    add(item: string): Promise<void>

    /**
     * Remove item from mru file.
     */

    remove(item: string): Promise<void>

    /**
     * Remove the data file.
     */
    clean(): Promise<void>
  }

  /**
   * Option to create task that runs in (neo)vim.
   */
  export interface TaskOptions {
    /**
     *  The command to run, without arguments
     */
    cmd: string
    /**
     * Arguments of command.
     */
    args?: string[]
    /**
     * Current working directory of the task, Default to current vim's cwd.
     */
    cwd?: string
    /**
     * Additional environment key-value pairs.
     */
    env?: { [key: string]: string }
    /**
     * Use pty when true.
     */
    pty?: boolean
    /**
     * Detach child process when true.
     */
    detach?: boolean
  }

  /**
   * Controls long running task started by (neo)vim.
   * Useful to keep the task running after CocRestart.
   */
  export interface Task extends Disposable {
    /**
     * Fired on task exit with exit code.
     */
    onExit: Event<number>
    /**
     * Fired with lines on stdout received.
     */
    onStdout: Event<string[]>
    /**
     * Fired with lines on stderr received.
     */
    onStderr: Event<string[]>
    /**
     * Start task, task will be restarted when already running.
     *
     * @param {TaskOptions} opts
     * @returns {Promise<boolean>}
     */
    start(opts: TaskOptions): Promise<boolean>
    /**
     * Stop task by SIGTERM or SIGKILL
     */
    stop(): Promise<void>
    /**
     * Check if the task is running.
     */
    running: Promise<boolean>
  }

  /**
   * A simple json database.
   */
  export interface JsonDB {
    filepath: string
    /**
     * Get data by key.
     *
     * @param {string} key unique key allows dot notation.
     * @returns {any}
     */
    fetch(key: string): any
    /**
     * Check if key exists
     *
     * @param {string} key unique key allows dot notation.
     */
    exists(key: string): boolean
    /**
     * Delete data by key
     *
     * @param {string} key unique key allows dot notation.
     */
    delete(key: string): void
    /**
     * Save data with key
     */
    push(key: string, data: number | null | boolean | string | { [index: string]: any }): void
    /**
     * Empty db file.
     */
    clear(): void
    /**
     * Remove db file.
     */
    destroy(): void
  }

  export interface RenameEvent {
    oldUri: Uri
    newUri: Uri
  }

  export interface FileSystemWatcher {
    readonly ignoreCreateEvents: boolean
    readonly ignoreChangeEvents: boolean
    readonly ignoreDeleteEvents: boolean
    readonly onDidCreate: Event<Uri>
    readonly onDidChange: Event<Uri>
    readonly onDidDelete: Event<Uri>
    readonly onDidRename: Event<RenameEvent>
    dispose(): void
  }

  export interface ConfigurationInspect<T> {
    key: string
    defaultValue?: T
    globalValue?: T
    workspaceValue?: T
  }

  export interface WorkspaceConfiguration {
    /**
     * Return a value from this configuration.
     *
     * @param section Configuration name, supports _dotted_ names.
     * @return The value `section` denotes or `undefined`.
     */
    get<T>(section: string): T | undefined

    /**
     * Return a value from this configuration.
     *
     * @param section Configuration name, supports _dotted_ names.
     * @param defaultValue A value should be returned when no value could be found, is `undefined`.
     * @return The value `section` denotes or the default.
     */
    get<T>(section: string, defaultValue: T): T

    /**
     * Check if this configuration has a certain value.
     *
     * @param section Configuration name, supports _dotted_ names.
     * @return `true` if the section doesn't resolve to `undefined`.
     */
    has(section: string): boolean

    /**
     * Retrieve all information about a configuration setting. A configuration value
     * often consists of a *default* value, a global or installation-wide value,
     * a workspace-specific value
     *
     * *Note:* The configuration name must denote a leaf in the configuration tree
     * (`editor.fontSize` vs `editor`) otherwise no result is returned.
     *
     * @param section Configuration name, supports _dotted_ names.
     * @return Information about a configuration setting or `undefined`.
     */
    inspect<T>(section: string): ConfigurationInspect<T> | undefined
    /**
     * Update a configuration value. The updated configuration values are persisted.
     *
     *
     * @param section Configuration name, supports _dotted_ names.
     * @param value The new value.
     * @param isUser if true, always update user configuration
     */
    update(section: string, value: any, isUser?: boolean): void

    /**
     * Readable dictionary that backs this configuration.
     */
    readonly [key: string]: any
  }

  export interface BufferSyncItem {
    /**
     * Called on buffer unload.
     */
    dispose: () => void
    /**
     * Called on buffer content change.
     */
    onChange?(e: DidChangeTextDocumentParams): void
    /**
     * Called on TextChangedI & TextChanged events.
     */
    onTextChange?(): void
  }

  export interface BufferSync<T extends BufferSyncItem> {
    /**
     * Current items.
     */
    readonly items: Iterable<T>
    /**
     * Get created item by uri
     */
    getItem(uri: string): T | undefined
    /**
     * Get created item by bufnr
     */
    getItem(bufnr: number): T | undefined
    dispose: () => void
  }

  export namespace workspace {
    export const nvim: Neovim
    /**
     * Current buffer number, could be wrong since vim could not send autocmd as expected.
     *
     * @deprecated will be removed in the feature.
     */
    export const bufnr: number
    /**
     * Current document.
     */
    export const document: Promise<Document>
    /**
     * Environments or current (neo)vim.
     */
    export const env: Env
    /**
     * Float window or popup can work.
     */
    export const floatSupported: boolean
    /**
     * Current working directory of vim.
     */
    export const cwd: string
    /**
     * Current workspace root.
     */
    export const root: string
    /**
     * @deprecated aliased to root.
     */
    export const rootPath: string
    /**
     * Not neovim when true.
     */
    export const isVim: boolean
    /**
     * Is neovim when true.
     */
    export const isNvim: boolean
    /**
     * All filetypes of loaded documents.
     */
    export const filetypes: ReadonlySet<string>
    /**
     * All languageIds of loaded documents.
     */
    export const languageIds: ReadonlySet<string>
    /**
     * Root directory of coc.nvim
     */
    export const pluginRoot: string
    /**
     * Current `&completeopt` of vim, may not correct.
     */
    export const completeOpt: string
    /**
     * Exists channel names.
     */
    export const channelNames: ReadonlyArray<string>
    /**
     * Loaded documents that attached.
     */
    export const documents: ReadonlyArray<Document>
    /**
     * Current document array.
     */
    export const textDocuments: ReadonlyArray<LinesTextDocument>
    /**
     * Current workspace folders.
     */
    export const workspaceFolders: ReadonlyArray<WorkspaceFolder>
    /**
     * Directory paths of workspaceFolders.
     */
    export const folderPaths: ReadonlyArray<string>
    /**
     * Current workspace folder, could be null when vim started from user's home.
     *
     * @deprecated
     */
    export const workspaceFolder: WorkspaceFolder | null
    export const onDidCreateFiles: Event<FileCreateEvent>
    export const onDidRenameFiles: Event<FileRenameEvent>
    export const onDidDeleteFiles: Event<FileDeleteEvent>
    export const onWillCreateFiles: Event<FileWillCreateEvent>
    export const onWillRenameFiles: Event<FileWillRenameEvent>
    export const onWillDeleteFiles: Event<FileWillDeleteEvent>
    /**
     * Event fired on workspace folder change.
     */
    export const onDidChangeWorkspaceFolders: Event<WorkspaceFoldersChangeEvent>
    /**
     * Event fired after document create.
     */
    export const onDidOpenTextDocument: Event<LinesTextDocument & { bufnr: number }>
    /**
     * Event fired after document unload.
     */
    export const onDidCloseTextDocument: Event<LinesTextDocument & { bufnr: number }>
    /**
     * Event fired on document change.
     */
    export const onDidChangeTextDocument: Event<DidChangeTextDocumentParams>
    /**
     * Event fired before document save.
     */
    export const onWillSaveTextDocument: Event<WillSaveEvent>
    /**
     * Event fired after document save.
     */
    export const onDidSaveTextDocument: Event<LinesTextDocument>

    /**
     * Event fired on configuration change. Configuration change could by many
     * reasons, including:
     *
     * - Changes detected from `coc-settings.json`.
     * - Change to document that using another configuration file.
     * - Configuration change by call update API of WorkspaceConfiguration.
     */
    export const onDidChangeConfiguration: Event<ConfigurationChangeEvent>

    /**
     * Fired when vim's runtimepath change detected.
     */
    export const onDidRuntimePathChange: Event<ReadonlyArray<string>>

    /**
     * Returns a path that is relative to the workspace folder or folders.
     *
     * When there are no {@link workspace.workspaceFolders workspace folders} or when the path
     * is not contained in them, the input is returned.
     *
     * @param pathOrUri A path or uri. When a uri is given its {@link Uri.fsPath fsPath} is used.
     * @param includeWorkspaceFolder When `true` and when the given path is contained inside a
     * workspace folder the name of the workspace is prepended. Defaults to `true` when there are
     * multiple workspace folders and `false` otherwise.
     * @return A path relative to the root or the input.
     */
    export function asRelativePath(pathOrUri: string | Uri, includeWorkspaceFolder?: boolean): string

    /**
     * Opens a document. Will return early if this document is already open. Otherwise
     * the document is loaded and the {@link workspace.onDidOpenTextDocument didOpen}-event fires.
     *
     * The document is denoted by an {@link Uri}. Depending on the {@link Uri.scheme scheme} the
     * following rules apply:
     * * `file`-scheme: Open a file on disk (`openTextDocument(Uri.file(path))`). Will be rejected if the file
     * does not exist or cannot be loaded.
     * * `untitled`-scheme: Open a blank untitled file with associated path (`openTextDocument(Uri.file(path).with({ scheme: 'untitled' }))`).
     * The language will be derived from the file name.
     * * For all other schemes contributed {@link TextDocumentContentProvider text document content providers} and
     * {@link FileSystemProvider file system providers} are consulted.
     *
     * *Note* that the lifecycle of the returned document is owned by the editor and not by the extension. That means an
     * {@linkcode workspace.onDidCloseTextDocument onDidClose}-event can occur at any time after opening it.
     *
     * @param uri Identifies the resource to open.
     * @return A promise that resolves to a {@link Document document}.
     */
    export function openTextDocument(uri: Uri): Thenable<Document>

    /**
     * A short-hand for `openTextDocument(Uri.file(fileName))`.
     *
     * @see {@link openTextDocument}
     * @param fileName A name of a file on disk.
     * @return A promise that resolves to a {@link Document document}.
     */
    export function openTextDocument(fileName: string): Thenable<Document>

    /**
     * Create new namespace id by name.
     *
     * @deprecated Latest neovim requires namespace created by neoivm.
     */
    export function createNameSpace(name: string): number

    /**
     * Like vim's has(), but for version check only.
     * Check patch on neovim and check nvim on vim would return false.
     *
     * For example:
     * - has('nvim-0.6.0')
     * - has('patch-7.4.248')
     */
    export function has(feature: string): boolean

    /**
     * Register autocmd on vim.
     *
     * Note: avoid request autocmd when possible since vim could be blocked
     * forever when request triggered during request.
     */
    export function registerAutocmd(autocmd: Autocmd): Disposable

    /**
     * Watch for vim's global option change.
     */
    export function watchOption(key: string, callback: (oldValue: any, newValue: any) => Thenable<void> | void, disposables?: Disposable[]): void

    /**
     * Watch for vim's global variable change, works on neovim only.
     */
    export function watchGlobal(key: string, callback?: (oldValue: any, newValue: any) => Thenable<void> | void, disposables?: Disposable[]): void

    /**
     * Check if selector match document.
     */
    export function match(selector: DocumentSelector, document: LinesTextDocument): number

    /**
     * Findup from filename or filenames from current filepath or root.
     *
     * @return fullpath of file or null when not found.
     */
    export function findUp(filename: string | string[]): Promise<string | null>

    /**
     * Get possible watchman binary path.
     */
    export function getWatchmanPath(): string | null

    /**
     * Get configuration by section and optional resource uri.
     */
    export function getConfiguration(section?: string, resource?: string): WorkspaceConfiguration

    /**
     * Get created document by uri or bufnr.
     */
    export function getDocument(uri: number | string): Document

    /**
     * Apply WorkspaceEdit.
     */
    export function applyEdit(edit: WorkspaceEdit): Promise<boolean>

    /**
     * Convert location to quickfix item.
     */
    export function getQuickfixItem(loc: Location | LocationLink, text?: string, type?: string, module?: string): Promise<QuickfixItem>

    /**
     * Convert locations to quickfix list.
     */
    export function getQuickfixList(locations: Location[]): Promise<ReadonlyArray<QuickfixItem>>

    /**
     * Populate locations to UI.
     */
    export function showLocations(locations: Location[]): Promise<void>

    /**
     * Get content of line by uri and line.
     */
    export function getLine(uri: string, line: number): Promise<string>

    /**
     * Get WorkspaceFolder of uri
     */
    export function getWorkspaceFolder(uri: string): WorkspaceFolder | undefined

    /**
     * Get content from buffer or file by uri.
     */
    export function readFile(uri: string): Promise<string>

    /**
     * Get current document and position.
     */
    export function getCurrentState(): Promise<EditerState>

    /**
     * Get format options of uri or current buffer.
     */
    export function getFormatOptions(uri?: string): Promise<FormattingOptions>

    /**
     * Jump to location.
     */
    export function jumpTo(uri: string, position?: Position | null, openCommand?: string): Promise<void>

    /**
     * Create a file in vim and disk
     */
    export function createFile(filepath: string, opts?: CreateFileOptions): Promise<void>

    /**
     * Load uri as document, buffer would be invisible if not loaded.
     */
    export function loadFile(uri: string): Promise<Document>

    /**
     * Load the files that not loaded
     */
    export function loadFiles(uris: string[]): Promise<void>

    /**
     * Rename file in vim and disk
     */
    export function renameFile(oldPath: string, newPath: string, opts?: RenameFileOptions): Promise<void>

    /**
     * Delete file from vim and disk.
     */
    export function deleteFile(filepath: string, opts?: DeleteFileOptions): Promise<void>

    /**
     * Open resource by uri
     */
    export function openResource(uri: string): Promise<void>

    /**
     * Resolve full path of module from yarn or npm global directory.
     */
    export function resolveModule(name: string): Promise<string>

    /**
     * Run nodejs command
     */
    export function runCommand(cmd: string, cwd?: string, timeout?: number): Promise<string>

    /**
     * Expand filepath with `~` and/or environment placeholders
     */
    export function expand(filepath: string): string

    /**
     * Call a function by use notifications, useful for functions like |input| that could block vim.
     */
    export function callAsync<T>(method: string, args: any[]): Promise<T>

    /**
     * registerTextDocumentContentProvider
     */
    export function registerTextDocumentContentProvider(scheme: string, provider: TextDocumentContentProvider): Disposable

    /**
     * Register unique keymap uses `<Plug>(coc-{key})` as lhs
     * Throw error when {key} already exists.
     *
     * @param {MapMode[]} modes - array of 'n' | 'i' | 'v' | 'x' | 's' | 'o'
     * @param {string} key - unique name
     * @param {Function} fn - callback function
     * @param {Partial} opts
     * @returns {Disposable}
     */
    export function registerKeymap(modes: MapMode[], key: string, fn: () => ProviderResult<any>, opts?: Partial<KeymapOption>): Disposable

    /**
     * Register expr key-mapping.
     */
    export function registerExprKeymap(mode: 'i' | 'n' | 'v' | 's' | 'x', key: string, fn: () => ProviderResult<string>, buffer?: boolean): Disposable

    /**
     * Register local key-mapping.
     */
    export function registerLocalKeymap(mode: 'n' | 'v' | 's' | 'x', key: string, fn: () => ProviderResult<any>, notify?: boolean): Disposable

    /**
     * Register for buffer sync objects, created item should be disposable
     * and provide optional `onChange` which called when document change.
     *
     * The document is always attached and not command line buffer.
     *
     * @param create Called for each attached document and on document create.
     * @returns Disposable
     */
    export function registerBufferSync<T extends BufferSyncItem>(create: (doc: Document) => T | undefined): BufferSync<T>

    /**
     * Create a FileSystemWatcher instance, when watchman doesn't exist, the
     * returned FileSystemWatcher can still be used, but not work at all.
     */
    export function createFileSystemWatcher(globPattern: string, ignoreCreate?: boolean, ignoreChange?: boolean, ignoreDelete?: boolean): FileSystemWatcher
    /**
     * Find files across all {@link workspace.workspaceFolders workspace folders} in the workspace.
     *
     * @example
     * findFiles('**​/*.js', '**​/node_modules/**', 10)
     *
     * @param include A {@link GlobPattern glob pattern} that defines the files to search for. The glob pattern
     * will be matched against the file paths of resulting matches relative to their workspace.
     * Use a {@link RelativePattern relative pattern} to restrict the search results to a {@link WorkspaceFolder workspace folder}.
     * @param exclude  A {@link GlobPattern glob pattern} that defines files and folders to exclude. The glob pattern
     * will be matched against the file paths of resulting matches relative to their workspace. When `undefined` or`null`,
     * no excludes will apply.
     * @param maxResults An upper-bound for the result.
     * @param token A token that can be used to signal cancellation to the underlying search engine.
     * @return A thenable that resolves to an array of resource identifiers. Will return no results if no
     * {@link workspace.workspaceFolders workspace folders} are opened.
     */
    export function findFiles(include: GlobPattern, exclude?: GlobPattern | null, maxResults?: number, token?: CancellationToken): Thenable<Uri[]>

    /**
     * Create persistence Mru instance.
     */
    export function createMru(name: string): Mru

    /**
     * Create Task instance that runs in (neo)vim, no shell.
     *
     * @param id Unique id string, like `TSC`
     */
    export function createTask(id: string): Task

    /**
     * Create DB instance at extension root.
     */
    export function createDatabase(name: string): JsonDB
  }
  // }}

  // window module {{
  /**
  * Represents how a terminal exited.
  */
  export interface TerminalExitStatus {
    /**
      * The exit code that a terminal exited with, it can have the following values:
      * - Zero: the terminal process or custom execution succeeded.
      * - Non-zero: the terminal process or custom execution failed.
      * - `undefined`: the user forcibly closed the terminal or a custom execution exited
      *   without providing an exit code.
      */
    readonly code: number | undefined
  }

  export interface TerminalOptions {
    /**
     * A human-readable string which will be used to represent the terminal in the UI.
     */
    name?: string

    /**
     * A path to a custom shell executable to be used in the terminal.
     */
    shellPath?: string

    /**
     * Args for the custom shell executable, this does not work on Windows (see #8429)
     */
    shellArgs?: string[]

    /**
     * A path or URI for the current working directory to be used for the terminal.
     */
    cwd?: string

    /**
     * Object with environment variables that will be added to the VS Code process.
     */
    env?: { [key: string]: string | null }

    /**
     * Whether the terminal process environment should be exactly as provided in
     * `TerminalOptions.env`. When this is false (default), the environment will be based on the
     * window's environment and also apply configured platform settings like
     * `terminal.integrated.windows.env` on top. When this is true, the complete environment
     * must be provided as nothing will be inherited from the process or any configuration.
     * Neovim only.
     */
    strictEnv?: boolean
  }

  /**
   * An individual terminal instance within the integrated terminal.
   */
  export interface Terminal {

    /**
     * The bufnr of terminal buffer.
     */
    readonly bufnr: number

    /**
     * The name of the terminal.
     */
    readonly name: string

    /**
     * The process ID of the shell process.
     */
    readonly processId: Promise<number>

    /**
     * The exit status of the terminal, this will be undefined while the terminal is active.
     *
     * **Example:** Show a notification with the exit code when the terminal exits with a
     * non-zero exit code.
     * ```typescript
     * window.onDidCloseTerminal(t => {
     *   if (t.exitStatus && t.exitStatus.code) {
     *   	vscode.window.showInformationMessage(`Exit code: ${t.exitStatus.code}`);
     *   }
     * });
     * ```
     */
    readonly exitStatus: TerminalExitStatus | undefined

    /**
     * Send text to the terminal. The text is written to the stdin of the underlying pty process
     * (shell) of the terminal.
     *
     * @param text The text to send.
     * @param addNewLine Whether to add a new line to the text being sent, this is normally
     * required to run a command in the terminal. The character(s) added are \n or \r\n
     * depending on the platform. This defaults to `true`.
     */
    sendText(text: string, addNewLine?: boolean): void

    /**
     * Show the terminal panel and reveal this terminal in the UI, return false when failed.
     *
     * @param preserveFocus When `true` the terminal will not take focus.
     */
    show(preserveFocus?: boolean): Promise<boolean>

    /**
     * Hide the terminal panel if this terminal is currently showing.
     */
    hide(): void

    /**
     * Dispose and free associated resources.
     */
    dispose(): void
  }

  /**
   * Option for create status item.
   */
  export interface StatusItemOption {
    progress?: boolean
  }

  /**
   * Status item that included in `g:coc_status`
   */
  export interface StatusBarItem {
    /**
     * The priority of this item. Higher value means the item should
     * be shown more to the left.
     */
    readonly priority: number

    isProgress: boolean

    /**
     * The text to show for the entry. You can embed icons in the text by leveraging the syntax:
     *
     * `My text $(icon-name) contains icons like $(icon-name) this one.`
     *
     * Where the icon-name is taken from the [octicon](https://octicons.github.com) icon set, e.g.
     * `light-bulb`, `thumbsup`, `zap` etc.
     */
    text: string

    /**
     * Shows the entry in the status bar.
     */
    show(): void

    /**
     * Hide the entry in the status bar.
     */
    hide(): void

    /**
     * Dispose and free associated resources. Call
     * [hide](#StatusBarItem.hide).
     */
    dispose(): void
  }

  /**
   * Value-object describing where and how progress should show.
   */
  export interface ProgressOptions {

    /**
     * A human-readable string which will be used to describe the
     * operation.
     */
    title?: string

    /**
     * Controls if a cancel button should show to allow the user to
     * cancel the long running operation.
     */
    cancellable?: boolean
  }

  /**
   * Defines a generalized way of reporting progress updates.
   */
  export interface Progress<T> {

    /**
     * Report a progress update.
     *
     * @param value A progress item, like a message and/or an
     * report on how much work finished
     */
    report(value: T): void
  }

  /**
   * Represents an action that is shown with an information, warning, or
   * error message.
   *
   * @see [showInformationMessage](#window.showInformationMessage)
   * @see [showWarningMessage](#window.showWarningMessage)
   * @see [showErrorMessage](#window.showErrorMessage)
   */
  export interface MessageItem {

    /**
     * A short title like 'Retry', 'Open Log' etc.
     */
    title: string

    /**
     * A hint for modal dialogs that the item should be triggered
     * when the user cancels the dialog (e.g. by pressing the ESC
     * key).
     *
     * Note: this option is ignored for non-modal messages.
     * Note: not used by coc.nvim for now.
     */
    isCloseAffordance?: boolean
  }

  export interface DialogButton {
    /**
     * Use by callback, should >= 0
     */
    index: number
    text: string
    /**
     * Not shown when true
     */
    disabled?: boolean
  }

  export interface DialogConfig {
    /**
     * Content shown in window.
     */
    content: string
    /**
     * Optional title text.
     */
    title?: string
    /**
     * show close button, default to true when not specified.
     */
    close?: boolean
    /**
     * highlight group for dialog window, default to `"dialog.floatHighlight"` or 'CocFlating'
     */
    highlight?: string
    /**
     * highlight items of content.
     */
    highlights?: ReadonlyArray<HighlightItem>
    /**
     * highlight groups for border, default to `"dialog.borderhighlight"` or 'CocFlating'
     */
    borderhighlight?: string
    /**
     * Buttons as bottom of dialog.
     */
    buttons?: DialogButton[]
    /**
     * index is -1 for window close without button click
     */
    callback?: (index: number) => void
  }

  export type NotificationKind = 'error' | 'info' | 'warning' | 'progress'

  export interface NotificationConfig {
    kind?: NotificationKind

    content?: string
    /**
     * Optional title text.
     */
    title?: string
    /**
     * Buttons as bottom of dialog.
     */
    buttons?: DialogButton[]
    /**
     * index is -1 for window close without button click
     */
    callback?: (index: number) => void
  }

  /**
   * Options to configure the behavior of the quick pick UI.
   */
  export interface QuickPickOptions {

    /**
     * An optional string that represents the title of the quick pick.
     */
    title?: string

    /**
     * An optional flag to include the description when filtering the picks.
     */
    matchOnDescription?: boolean

    /**
     * An optional flag to make the picker accept multiple selections, if true the result is an array of picks.
     */
    canPickMany?: boolean
  }

  /**
   * Represents an item that can be selected from
   * a list of items.
   */
  export interface QuickPickItem {
    /**
     * A human-readable string which is rendered prominent
     */
    label: string
    /**
     * A human-readable string which is rendered less prominent in the same line
     */
    description?: string
    /**
     * Optional flag indicating if this item is picked initially.
     */
    picked?: boolean
  }

  export interface QuickPickConfig<T extends QuickPickItem> {
    /**
     * An optional title.
     */
    title?: string
    /**
     * Items to pick from.
     */
    items: readonly T[]
    /**
     * Initial value of the filter text.
     */
    value?: string
    /**
     * If multiple items can be selected at the same time. Defaults to false.
     */
    canSelectMany?: boolean
    /**
     * Max height of list window.
     */
    maxHeight?: number
  }

  export interface QuickPick<T extends QuickPickItem> {
    /**
     * An optional title.
     */
    title: string | undefined
    /**
     * If the UI should show a progress indicator. Defaults to false.
     *
     * Change this to true, e.g., while loading more data or validating
     * user input.
     */
    loading: boolean
    /**
     * Items to pick from. This can be read and updated by the extension.
     */
    items: readonly T[]
    /**
     * Active items. This can be read and updated by the extension.
     */
    activeItems: readonly T[]
    /**
     * If the filter text should also be matched against the description of the items. Defaults to false.
     */
    matchOnDescription: boolean
    /**
     * Current input value
     */
    readonly value: string
    /**
     * An event signaling when QuickPick closed, fired with selected items or null when canceled.
     */
    readonly onDidFinish: Event<T[] | null>
    /**
     * An event signaling when the value of the filter text has changed.
     */
    readonly onDidChangeValue: Event<string>
    /**
     * An event signaling when the selected items have changed.
     */
    readonly onDidChangeSelection: Event<readonly T[]>
  }

  export interface ScreenPosition {
    row: number
    col: number
  }

  export type MsgTypes = 'error' | 'warning' | 'more'

  export interface OpenTerminalOption {
    /**
     * Cwd of terminal, default to result of |getcwd()|
     */
    cwd?: string
    /**
     * Close terminal on job finish, default to true.
     */
    autoclose?: boolean
    /**
     * Keep focus current window, default to false.
     */
    keepfocus?: boolean
    /**
     * Position of terminal window, default to 'right'.
     */
    position?: 'bottom' | 'right'
  }

  /**
   * An output channel is a container for readonly textual information.
   *
   * To get an instance of an `OutputChannel` use
   * [createOutputChannel](#window.createOutputChannel).
   */
  export interface OutputChannel {

    /**
     * The human-readable name of this output channel.
     */
    readonly name: string

    readonly content: string
    /**
     * Append the given value to the channel.
     *
     * @param value A string, falsy values will not be printed.
     */
    append(value: string): void

    /**
     * Append the given value and a line feed character
     * to the channel.
     *
     * @param value A string, falsy values will be printed.
     */
    appendLine(value: string): void

    /**
     * Removes output from the channel. Latest `keep` lines will be remained.
     */
    clear(keep?: number): void

    /**
     * Reveal this channel in the UI.
     *
     * @param preserveFocus When `true` the channel will not take focus.
     */
    show(preserveFocus?: boolean): void

    /**
     * Hide this channel from the UI.
     */
    hide(): void

    /**
     * Dispose and free associated resources.
     */
    dispose(): void
  }

  export interface TerminalResult {
    bufnr: number
    success: boolean
    content?: string
  }

  export interface Dialog {
    /**
     * Buffer number of dialog.
     */
    bufnr: number
    /**
     * Window id of dialog.
     */
    winid: Promise<number | null>
    dispose: () => void
  }

  export type HighlightItemDef = [string, number, number, number, number?, number?, number?]

  export interface HighlightDiff {
    remove: number[]
    removeMarkers: number[]
    add: HighlightItemDef[]
  }

  export interface MenuItem {
    text: string
    disabled?: boolean | { reason: string }
  }

  export interface MenuOption {
    /**
     * Title in menu window.
     */
    title?: string,
    /**
     * Content in menu window as normal text.
     */
    content?: string
    /**
     * Create and highlight shortcut characters.
     */
    shortcuts?: boolean
    /**
     * Position of menu, default to 'cursor'
     */
    position?: 'center' | 'cursor'
  }

  export interface InputOptions {
    /**
     * Position to show input, default to 'cursor'
     */
    position?: 'cursor' | 'center'
    /**
     * Margin top editor when position is 'center'
     */
    marginTop?: number
    /**
     * Border highlight of float window/popup, configuration `dialog.borderhighlight` used as default.
     */
    borderhighlight?: string
    /**
     * Create key-mappings for quickpick list.
     */
    list?: boolean
  }

  export interface InputPreference extends InputOptions {
    /**
     * Top, right, bottom, left border existence, default to [1,1,1,1]
     */
    border?: [0 | 1, 0 | 1, 0 | 1, 0 | 1]
    /**
     * Rounded border, default to true, configuration `dialog.rounded` used as default.
     */
    rounded?: boolean
    /**
     * Minimal window width, `g:coc_prompt_win_width` or 32 used as default.
     */
    minWidth?: number
    /**
     * Maximum window width, configuration `dialog.maxWidth` used as default.
     */
    maxWidth?: number
  }

  export interface InputDimension {
    readonly width: number
    readonly height: number
    /**
     * 0 based screen row
     */
    readonly row: number
    /**
     * O based screen col
     */
    readonly col: number
  }

  export interface InputBox {
    /**
     * Change or get title of input box.
     */
    title: string
    /**
     * Change or get loading state of input box.
     */
    loading: boolean
    /**
     * Change or get borderhighlight of input box.
     */
    borderhighlight: string
    /**
     * Dimension of float window/popup
     */
    readonly dimension: InputDimension
    /**
     * Buffer number of float window/popup
     */
    readonly bufnr: number
    /**
     * An event signaling when the value has changed.
     */
    readonly onDidChange: Event<string>
    /**
     * An event signaling input finished, emit input value or null when canceled.
     */
    readonly onDidFinish: Event<string | null>
  }

  export namespace window {
    /**
     * The currently active editor or `undefined`. The active editor is the one
     * that currently has focus or, when none has focus, the one that has changed
     * input most recently.
     */
    export const activeTextEditor: TextEditor | undefined

    /**
     * The currently visible editors or an empty array.
     */
    export const visibleTextEditors: readonly TextEditor[]

    /**
     * An {@link Event} which fires when the {@link window.activeTextEditor active editor}
     * has changed. *Note* that the event also fires when the active editor changes
     * to `undefined`.
     */
    export const onDidChangeActiveTextEditor: Event<TextEditor | undefined>

    /**
     * An {@link Event} which fires when the array of {@link window.visibleTextEditors visible editors}
     * has changed.
     */
    export const onDidChangeVisibleTextEditors: Event<readonly TextEditor[]>

    /**
     * The currently opened terminals or an empty array.
     */
    export const terminals: readonly Terminal[]
    /**
     * onDidChangeTerminalState doesn't exist since we can't detect window resize on vim.
     */
    /**
     * Event fired after terminal created, only fired with Terminal that
     * created by `window.createTerminal`
     */
    export const onDidOpenTerminal: Event<Terminal>
    /**
     * Event fired on terminal close, only fired with Terminal that created by
     * `window.createTerminal`
     */
    export const onDidCloseTerminal: Event<Terminal>
    /**
     * Creates a {@link Terminal} with a backing shell process.
     * The terminal is created by (neo)vim.
     *
     * @param options A TerminalOptions object describing the characteristics of the new terminal.
     * @return A new Terminal.
     * @throws When running in an environment where a new process cannot be started.
     */
    export function createTerminal(opts: TerminalOptions): Promise<Terminal>

    /**
     * Reveal message with message type.
     *
     * @deprecated Use `window.showErrorMessage`, `window.showWarningMessage` and `window.showInformationMessage` instead.
     * @param msg Message text to show.
     * @param messageType Type of message, could be `error` `warning` and `more`, default to `more`
     */
    export function showMessage(msg: string, messageType?: MsgTypes): void

    /**
     * Run command in vim terminal for result
     *
     * @param cmd Command to run.
     * @param cwd Cwd of terminal, default to result of |getcwd()|.
     */
    export function runTerminalCommand(cmd: string, cwd?: string, keepfocus?: boolean): Promise<TerminalResult>

    /**
     * Open terminal window.
     *
     * @param cmd Command to run.
     * @param opts Terminal option.
     * @returns buffer number of terminal.
     */
    export function openTerminal(cmd: string, opts?: OpenTerminalOption): Promise<number>

    /**
     * Show quickpick for single item, use `window.menuPick` for menu at current current position.
     * Use `window.showPickerDialog()` for multiple selection.
     *
     * @param items Label list.
     * @param placeholder Prompt text, default to 'choose by number'.
     * @returns Index of selected item, or -1 when canceled.
     */
    export function showQuickpick(items: string[], placeholder?: string): Promise<number>

    /**
     * Shows a selection list allowing multiple selections.
     * Throw error when 'workspace.env.dialog' is not true.
     *
     * @param items An array of strings, or a promise that resolves to an array of strings.
     * @param options Configures the behavior of the selection list.
     * @param token A token that can be used to signal cancellation.
     * @return A promise that resolves to the selected items or `undefined`.
     */
    export function showQuickPick(items: readonly string[] | Thenable<readonly string[]>, options: QuickPickOptions & { canPickMany: true }, token?: CancellationToken): Thenable<string[] | undefined>

    /**
     * Shows a selection list.
     * Throw error when 'workspace.env.dialog' is not true.
     *
     * @param items An array of strings, or a promise that resolves to an array of strings.
     * @param options Configures the behavior of the selection list.
     * @param token A token that can be used to signal cancellation.
     * @return A promise that resolves to the selection or `undefined`.
     */
    export function showQuickPick(items: readonly string[] | Thenable<readonly string[]>, options?: QuickPickOptions, token?: CancellationToken): Thenable<string | undefined>

    /**
     * Shows a selection list allowing multiple selections.
     * Throw error when 'workspace.env.dialog' is not true.
     *
     * @param items An array of items, or a promise that resolves to an array of items.
     * @param options Configures the behavior of the selection list.
     * @param token A token that can be used to signal cancellation.
     * @return A promise that resolves to the selected items or `undefined`.
     */
    export function showQuickPick<T extends QuickPickItem>(items: readonly T[] | Thenable<readonly T[]>, options: QuickPickOptions & { canPickMany: true }, token?: CancellationToken): Thenable<T[] | undefined>

    /**
     * Shows a selection list.
     * Throw error when `workspace.env.dialog` not true.
     *
     * @param items An array of items, or a promise that resolves to an array of items.
     * @param options Configures the behavior of the selection list.
     * @param token A token that can be used to signal cancellation.
     * @return A promise that resolves to the selected item or `undefined`.
     */
    export function showQuickPick<T extends QuickPickItem>(items: readonly T[] | Thenable<readonly T[]>, options?: QuickPickOptions, token?: CancellationToken): Thenable<T | undefined>

    /**
     * Show menu picker at current cursor position, |inputlist()| is used as fallback.
     * Throw error when `workspace.env.dialog` not true.
     *
     * @param items Array of texts or menu items.
     * @param title Optional title of float/popup window.
     * @param token A token that can be used to signal cancellation.
     * @returns Selected index (0 based), -1 when canceled.
     */
    export function showMenuPicker(items: string[] | MenuItem[], option?: MenuOption | string, token?: CancellationToken): Promise<number>

    /**
     * Prompt user for confirm, a float/popup window would be used when possible,
     * use vim's |confirm()| function as callback.
     *
     * @param title The prompt text.
     * @returns Result of confirm.
     */
    export function showPrompt(title: string): Promise<boolean>

    /**
     * Show dialog window at the center of screen.
     * Note that the dialog would always be closed after button click.
     * Throw error when `workspace.env.dialog` not true.
     *
     * @param config Dialog configuration.
     * @returns Dialog or null when dialog can't work.
     */
    export function showDialog(config: DialogConfig): Promise<Dialog | null>

    /**
     * Request input from user, `input()` is used when `window.env.dialog` not true.
     *
     * @param title Title text of prompt window.
     * @param defaultValue Default value of input, empty text by default.
     * @param {InputOptions} option for input window, other preferences are read from user configuration.
     */
    export function requestInput(title: string, defaultValue?: string, option?: InputOptions): Promise<string>

    /**
     * Creates and show a {@link InputBox} to let the user enter some text input.
     * Throw error when `workspace.env.dialog` not true.
     *
     * @return A new {@link InputBox}.
     */
    export function createInputBox(title: string, defaultValue: string | undefined, option: InputPreference): Promise<InputBox>

    /**
     * Creates and show a {@link QuickPick} to let the user pick an item or items from a
     * list of items of type T.
     * Throw error when `workspace.env.dialog` not true.
     *
     * Note that in many cases the more convenient {@link window.showQuickPick}
     * is easier to use. {@link window.createQuickPick} should be used
     * when {@link window.showQuickPick} does not offer the required flexibility.
     *
     * @return A new {@link QuickPick}.
     */
    export function createQuickPick<T extends QuickPickItem>(config: QuickPickConfig<T>): Promise<QuickPick<T>>

    /**
     * Create statusbar item that would be included in `g:coc_status`.
     *
     * @param priority Higher priority item would be shown right.
     * @param option
     * @return A new status bar item.
     */
    export function createStatusBarItem(priority?: number, option?: StatusItemOption): StatusBarItem

    /**
     * Open local config file
     */
    export function openLocalConfig(): Promise<void>

    /**
     * Create a new output channel
     *
     * @param name Unique name of output channel.
     * @returns A new output channel.
     */
    export function createOutputChannel(name: string): OutputChannel

    /**
     * Create a {@link TreeView} instance, call `show()` method to render.
     *
     * @param viewId Id of the view, used as title of TreeView when title doesn't exist.
     * @param options Options for creating the {@link TreeView}
     * @returns a {@link TreeView}.
     */
    export function createTreeView<T>(viewId: string, options: TreeViewOptions<T>): TreeView<T>

    /**
     * Reveal buffer of output channel.
     *
     * @param name Name of output channel.
     * @param preserveFocus Preserve window focus when true.
     */
    export function showOutputChannel(name: string, preserveFocus: boolean): void

    /**
     * Echo lines at the bottom of vim.
     *
     * @param lines Line list.
     * @param truncate Truncate the lines to avoid 'press enter to continue' when true
     */
    export function echoLines(lines: string[], truncate?: boolean): Promise<void>

    /**
     * Get current cursor position (line, character both 0 based).
     *
     * @returns Cursor position.
     */
    export function getCursorPosition(): Promise<Position>

    /**
     * Move cursor to position (line, character both 0 based).
     *
     * @param position LSP position.
     */
    export function moveTo(position: Position): Promise<void>

    /**
     * Get current cursor character offset in document,
     * length of line break would always be 1.
     *
     * @returns Character offset.
     */
    export function getOffset(): Promise<number>

    /**
     * Get screen position of current cursor(relative to editor),
     * both `row` and `col` are 0 based.
     *
     * @returns Cursor screen position.
     */
    export function getCursorScreenPosition(): Promise<ScreenPosition>

    /**
     * Show multiple picker at center of screen.
     * Use `workspace.env.dialog` to check if dialog could work.
     *
     * @param items A set of items that will be rendered as actions in the message.
     * @param title Title of picker dialog.
     * @param token A token that can be used to signal cancellation.
     * @return A promise that resolves to the selected items or `undefined`.
     */
    export function showPickerDialog(items: string[], title: string, token?: CancellationToken): Promise<string[] | undefined>

    /**
     * Show multiple picker at center of screen.
     * Use `workspace.env.dialog` to check if dialog could work.
     *
     * @param items A set of items that will be rendered as actions in the message.
     * @param title Title of picker dialog.
     * @param token A token that can be used to signal cancellation.
     * @return A promise that resolves to the selected items or `undefined`.
     */
    export function showPickerDialog<T extends QuickPickItem>(items: T[], title: string, token?: CancellationToken): Promise<T[] | undefined>

    /**
     * Show an information message to users. Optionally provide an array of items which will be presented as
     * clickable buttons.
     *
     * @param message The message to show.
     * @param items A set of items that will be rendered as actions in the message.
     * @return Promise that resolves to the selected item or `undefined` when being dismissed.
     */
    export function showInformationMessage(message: string, ...items: string[]): Promise<string | undefined>
    /**
     * Show an information message to users. Optionally provide an array of items which will be presented as
     * clickable buttons.
     *
     * @param message The message to show.
     * @param items A set of items that will be rendered as actions in the message.
     * @return Promise that resolves to the selected item or `undefined` when being dismissed.
     */
    export function showInformationMessage<T extends MessageItem>(message: string, ...items: T[]): Promise<T | undefined>

    /**
     * Show an warning message to users. Optionally provide an array of items which will be presented as
     * clickable buttons.
     *
     * @param message The message to show.
     * @param items A set of items that will be rendered as actions in the message.
     * @return Promise that resolves to the selected item or `undefined` when being dismissed.
     */
    export function showWarningMessage(message: string, ...items: string[]): Promise<string | undefined>
    /**
     * Show an warning message to users. Optionally provide an array of items which will be presented as
     * clickable buttons.
     *
     * @param message The message to show.
     * @param items A set of items that will be rendered as actions in the message.
     * @return Promise that resolves to the selected item or `undefined` when being dismissed.
     */
    export function showWarningMessage<T extends MessageItem>(message: string, ...items: T[]): Promise<T | undefined>

    /**
     * Show an error message to users. Optionally provide an array of items which will be presented as
     * clickable buttons.
     *
     * @param message The message to show.
     * @param items A set of items that will be rendered as actions in the message.
     * @return Promise that resolves to the selected item or `undefined` when being dismissed.
     */
    export function showErrorMessage(message: string, ...items: string[]): Promise<string | undefined>
    /**
     * Show an error message to users. Optionally provide an array of items which will be presented as
     * clickable buttons.
     *
     * @param message The message to show.
     * @param items A set of items that will be rendered as actions in the message.
     * @return Promise that resolves to the selected item or `undefined` when being dismissed.
     */
    export function showErrorMessage<T extends MessageItem>(message: string, ...items: T[]): Promise<T | undefined>

    /**
     * Show notification window at bottom right of screen.
     */
    export function showNotification(config: NotificationConfig): Promise<void>

    /**
     * Show progress in the editor. Progress is shown while running the given callback
     * and while the promise it returned isn't resolved nor rejected.
     *
     * @param task A callback returning a promise. Progress state can be reported with
     * the provided [progress](#Progress)-object.
     *
     * To report discrete progress, use `increment` to indicate how much work has been completed. Each call with
     * a `increment` value will be summed up and reflected as overall progress until 100% is reached (a value of
     * e.g. `10` accounts for `10%` of work done).
     *
     * To monitor if the operation has been cancelled by the user, use the provided [`CancellationToken`](#CancellationToken).
     *
     * @return The thenable the task-callback returned.
     */
    export function withProgress<R>(options: ProgressOptions, task: (progress: Progress<{
      message?: string
      increment?: number
    }>, token: CancellationToken) => Thenable<R>): Promise<R>

    /**
     * Get selected range for current document
     */
    export function getSelectedRange(visualmode: string): Promise<Range | null>

    /**
     * Visual select range of current document
     */
    export function selectRange(range: Range): Promise<void>

    /**
     * Get diff between new highlight items and current highlights requested from vim
     *
     * @param {number} bufnr - Buffer number
     * @param {string} ns - Highlight namespace
     * @param {HighlightItem[]} items - Highlight items
     * @param {[number, number] | undefined} - region 0 based start line and end line (end exclusive)
     * @param {CancellationToken} token - CancellationToken
     * @returns {Promise<HighlightDiff>}
     */
    export function diffHighlights(bufnr: number, ns: string, items: ExtendedHighlightItem[], region?: [number, number] | undefined, token?: CancellationToken): Promise<HighlightDiff | null>

    /**
     * Apply highlight diffs, normally used with `window.diffHighlights`
     *
     * Timer is used to add highlights when there're too many highlight items to add,
     * the highlight process won't be finished on that case.
     *
     * @param {number} bufnr - Buffer name
     * @param {string} ns - Namespace
     * @param {number} priority
     * @param {HighlightDiff} diff
     * @param {boolean} notify - Use notification, default false.
     * @returns {Promise<void>}
     */
    export function applyDiffHighlights(bufnr: number, ns: string, priority: number, diff: HighlightDiff, notify?: boolean): Promise<void>
  }
  // }}

  // extensions module {{
  export interface Logger {
    readonly category: string
    readonly level: string
    log(...args: any[]): void
    trace(message: any, ...args: any[]): void
    debug(message: any, ...args: any[]): void
    info(message: any, ...args: any[]): void
    warn(message: any, ...args: any[]): void
    error(message: any, ...args: any[]): void
    fatal(message: any, ...args: any[]): void
    mark(message: any, ...args: any[]): void
  }

  /**
   * A memento represents a storage utility. It can store and retrieve
   * values.
   */
  export interface Memento {

    /**
     * Return a value.
     *
     * @param key A string.
     * @return The stored value or `undefined`.
     */
    get<T>(key: string): T | undefined

    /**
     * Return a value.
     *
     * @param key A string.
     * @param defaultValue A value that should be returned when there is no
     * value (`undefined`) with the given key.
     * @return The stored value or the defaultValue.
     */
    get<T>(key: string, defaultValue: T): T

    /**
     * Store a value. The value must be JSON-stringifyable.
     *
     * @param key A string.
     * @param value A value. MUST not contain cyclic references.
     */
    update(key: string, value: any): Promise<void>
  }

  export type ExtensionState = 'disabled' | 'loaded' | 'activated' | 'unknown'

  export enum ExtensionType {
    Global,
    Local,
    SingleFile,
    Internal
  }

  export interface ExtensionJson {
    name: string
    main?: string
    engines: {
      [key: string]: string
    }
    version?: string
    [key: string]: any
  }

  export interface ExtensionInfo {
    id: string
    version: string
    description: string
    root: string
    exotic: boolean
    uri?: string
    state: ExtensionState
    isLocal: boolean
    packageJSON: Readonly<ExtensionJson>
  }

  /**
   * Represents an extension.
   *
   * To get an instance of an `Extension` use [getExtension](#extensions.getExtension).
   */
  export interface Extension<T> {

    /**
     * The canonical extension identifier in the form of: `publisher.name`.
     */
    readonly id: string

    /**
     * The absolute file path of the directory containing this extension.
     */
    readonly extensionPath: string

    /**
     * `true` if the extension has been activated.
     */
    readonly isActive: boolean

    /**
     * The parsed contents of the extension's package.json.
     */
    readonly packageJSON: any

    /**
     * The public API exported by this extension. It is an invalid action
     * to access this field before this extension has been activated.
     */
    readonly exports: T

    /**
     * Activates this extension and returns its public API.
     *
     * @return A promise that will resolve when this extension has been activated.
     */
    activate(): Promise<T>
  }

  /**
   * An extension context is a collection of utilities private to an
   * extension.
   *
   * An instance of an `ExtensionContext` is provided as the first
   * parameter to the `activate`-call of an extension.
   */
  export interface ExtensionContext {

    /**
     * An array to which disposables can be added. When this
     * extension is deactivated the disposables will be disposed.
     */
    subscriptions: Disposable[]

    /**
     * The absolute file path of the directory containing the extension.
     */
    extensionPath: string

    /**
     * Get the absolute path of a resource contained in the extension.
     *
     * @param relativePath A relative path to a resource contained in the extension.
     * @return The absolute path of the resource.
     */
    asAbsolutePath(relativePath: string): string

    /**
     * The absolute directory path for extension to download persist data.
     * The directory might not exist.
     */
    storagePath: string

    /**
     * A memento object that stores state in the context
     * of the currently opened [workspace](#workspace.workspaceFolders).
     */
    workspaceState: Memento

    /**
     * A memento object that stores state independent
     * of the current opened [workspace](#workspace.workspaceFolders).
     */
    globalState: Memento

    logger: Logger
  }

  export type ExtensionApi = {
    [index: string]: any
  } | void | null | undefined

  export interface PropertyScheme {
    type: string
    default: any
    description: string
    enum?: string[]
    items?: any
    [key: string]: any
  }

  export namespace extensions {
    /**
     * Fired on extension loaded, extension not activated yet.
     */
    export const onDidLoadExtension: Event<Extension<ExtensionApi>>

    /**
     * Fired on extension activated.
     */
    export const onDidActiveExtension: Event<Extension<ExtensionApi>>

    /**
     * Fired with extension id on extension unload.
     */
    export const onDidUnloadExtension: Event<string>

    /**
     * Get all loaded extensions, without disabled extensions, extension may not activated.
     */
    export const all: ReadonlyArray<Extension<ExtensionApi>>

    /**
     * Get state of specific extension.
     */
    export function getExtensionState(id: string): ExtensionState

    /**
     * Get state of all extensions, including disabled extensions.
     */
    export function getExtensionStates(): Promise<ExtensionInfo[]>

    /**
     * Check if extension is activated.
     */
    export function isActivated(id: string): boolean

    /**
     * Dynamic add custom json schemes without using package.json.
     */
    export function addSchemeProperty(key: string, def: PropertyScheme): void
  }
  // }}

  // listManager module {{
  export interface LocationWithLine {
    uri: string
    /**
     * Match text of line.
     */
    line: string
    /**
     * Highlight text in line.
     */
    text?: string
  }

  export interface AnsiHighlight {
    /**
     * Byte indexes, 0 based.
     */
    span: [number, number]
    hlGroup: string
  }

  export interface ListItem {
    label: string
    preselect?: boolean
    filterText?: string
    /**
     * A string that should be used when comparing this item
     * with other items, only used for fuzzy filter.
     */
    sortText?: string
    location?: Location | LocationWithLine | string
    data?: any
    ansiHighlights?: AnsiHighlight[]
    resolved?: boolean
  }

  export interface ListHighlights {
    /**
     * Byte indexes list, 0 based.
     */
    spans: [number, number][]
    /**
     * `CocListSearch` is used when it not exist.
     */
    hlGroup?: string
  }

  export type ListMode = 'normal' | 'insert'

  export type ListMatcher = 'strict' | 'fuzzy' | 'regex'

  export interface ListOptions {
    position: string
    reverse: boolean
    input: string
    ignorecase: boolean
    interactive: boolean
    sort: boolean
    mode: ListMode
    matcher: ListMatcher
    autoPreview: boolean
    numberSelect: boolean
    noQuit: boolean
    first: boolean
  }

  export interface ListContext {
    /**
     * Input on list activated.
     */
    input: string
    /**
     * Current work directory on activated.
     */
    cwd: string
    /**
     * Options of list.
     */
    options: ListOptions
    /**
     * Arguments passed to list.
     */
    args: string[]
    /**
     * Original window on list invoke.
     */
    window: Window
    /**
     * Original buffer on list invoke.
     */
    buffer: Buffer
    listWindow: Window | null
  }

  export interface ListAction {
    /**
     * Action name
     */
    name: string
    /**
     * Should persist list window on invoke.
     */
    persist?: boolean
    /**
     * Should reload list after invoke.
     */
    reload?: boolean
    /**
     * Inovke all selected items in parallel.
     */
    parallel?: boolean
    /**
     * Support handle multiple items at once.
     */
    multiple?: boolean
    /**
     * Tab positioned list should be persisted (no window switch) on action invoke.
     */
    tabPersist?: boolean
    /**
     * Item is array of selected items when multiple is true.
     */
    execute: (item: ListItem | ListItem[], context: ListContext) => ProviderResult<void>
  }

  export interface MultipleListAction extends Omit<ListAction, 'execute'> {
    multiple: true
    execute: (item: ListItem[], context: ListContext) => ProviderResult<void>
  }

  export interface ListTask {
    on(event: 'data', callback: (item: ListItem) => void): void
    on(event: 'end', callback: () => void): void
    on(event: 'error', callback: (msg: string | Error) => void): void
    dispose(): void
  }

  export interface ListArgument {
    key?: string
    hasValue?: boolean
    name: string
    description: string
  }

  export interface IList {
    /**
     * Unique name of list.
     */
    name: string
    /**
     * Default action name.
     */
    defaultAction: string
    /**
     * Action list.
     */
    actions: ListAction[]
    /**
     * Load list items.
     */
    loadItems(context: ListContext, token: CancellationToken): Promise<ListItem[] | ListTask | null | undefined>
    /**
     * Should be true when interactive is supported.
     */
    interactive?: boolean
    /**
     * Description of list.
     */
    description?: string
    /**
     * Detail description, shown in help.
     */
    detail?: string
    /**
     * Options supported by list.
     */
    options?: ListArgument[]
    /**
     * Resolve list item.
     */
    resolveItem?(item: ListItem): Promise<ListItem | null>
    /**
     * Highlight buffer by vim's syntax commands.
     */
    doHighlight?(): void
    /**
     * Called on list unregistered.
     */
    dispose?(): void
  }

  export interface PreviewOptions {
    bufname?: string
    lines: string[]
    filetype: string
    lnum?: number
    range?: Range
    /**
     * @deprecated not used
     */
    sketch?: boolean
  }

  export namespace listManager {
    /**
     * Registered list names set.
     */
    export const names: ReadonlyArray<string>
    /**
     * Register list, list session can be created by `CocList [name]` after registered.
     */
    export function registerList(list: IList): Disposable
  }
  // }}

  // snippetManager module {{
  export interface SnippetSession {
    isActive: boolean
  }

  export interface UltiSnippetOption {
    regex?: string
    context?: string
  }

  /**
   * A snippet string is a template which allows to insert text
   * and to control the editor cursor when insertion happens.
   *
   * A snippet can define tab stops and placeholders with `$1`, `$2`
   * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
   * the end of the snippet. Variables are defined with `$name` and
   * `${name:default value}`. The full snippet syntax is documented
   * [here](https://code.visualstudio.com/docs/editor/userdefinedsnippets#_creating-your-own-snippets).
   */
  export class SnippetString {
    /**
     * The snippet string.
     */
    value: string

    constructor(
      /**
       * The default snippet string.
       */
      value?: string
    )

    /**
     * Builder-function that appends the given string to
     * the {@link SnippetString.value `value`} of this snippet string.
     *
     * @param string A value to append 'as given'. The string will be escaped.
     * @return This snippet string.
     */
    appendText(string: string): SnippetString

    /**
     * Builder-function that appends a tabstop (`$1`, `$2` etc) to
     * the {@link SnippetString.value `value`} of this snippet string.
     *
     * @param number The number of this tabstop, defaults to an auto-increment
     * value starting at 1.
     * @return This snippet string.
     */
    appendTabstop(number?: number): SnippetString

    /**
     * Builder-function that appends a placeholder (`${1:value}`) to
     * the {@link SnippetString.value `value`} of this snippet string.
     *
     * @param value The value of this placeholder - either a string or a function
     * with which a nested snippet can be created.
     * @param number The number of this tabstop, defaults to an auto-increment
     * value starting at 1.
     * @return This snippet string.
     */
    appendPlaceholder(value: string | ((snippet: SnippetString) => any), number?: number): SnippetString

    /**
     * Builder-function that appends a choice (`${1|a,b,c|}`) to
     * the {@link SnippetString.value `value`} of this snippet string.
     *
     * @param values The values for choices - the array of strings
     * @param number The number of this tabstop, defaults to an auto-increment
     * value starting at 1.
     * @return This snippet string.
     */
    appendChoice(values: string[], number?: number): SnippetString

    /**
     * Builder-function that appends a variable (`${VAR}`) to
     * the {@link SnippetString.value `value`} of this snippet string.
     *
     * @param name The name of the variable - excluding the `$`.
     * @param defaultValue The default value which is used when the variable name cannot
     * be resolved - either a string or a function with which a nested snippet can be created.
     * @return This snippet string.
     */
    appendVariable(name: string, defaultValue: string | ((snippet: SnippetString) => any)): SnippetString
  }
  /**
   * Manage snippet sessions.
   */
  export namespace snippetManager {
    /**
     * Get snippet session by bufnr.
     */
    export function getSession(bufnr: number): SnippetSession | undefined
    /**
     * Resolve snippet string to text.
     */
    export function resolveSnippet(body: string, ultisnip?: UltiSnippetOption): Promise<string>
    /**
     * Insert snippet at current buffer.
     *
     * @param {string} snippet Textmate snippet string.
     * @param {boolean} select Not select first placeholder when false, default `true`.
     * @param {Range} range Repalce range, insert to current cursor position when undefined.
     * @returns {Promise<boolean>} true when insert success.
     */
    export function insertSnippet(snippet: string | SnippetString, select?: boolean, range?: Range, ultisnip?: boolean): Promise<boolean>

    /**
     * Jump to next placeholder, only works when snippet session activated.
     */
    export function nextPlaceholder(): Promise<void>
    /**
     * Jump to previous placeholder, only works when snippet session activated.
     */
    export function previousPlaceholder(): Promise<void>
    /**
     * Cancel snippet session of current buffer, does nothing when no session activated.
     */
    export function cancel(): void
    /**
     * Check if snippet activated for bufnr.
     */
    export function isActivated(bufnr: number): boolean
  }
  // }}

  // diagnosticManager module {{
  export interface DiagnosticItem {
    file: string
    lnum: number
    col: number
    source: string
    code: string | number
    message: string
    severity: string
    level: number
    location: Location
  }

  export enum DiagnosticKind {
    Syntax,
    Semantic,
    Suggestion,
  }

  /**
   * A diagnostics collection is a container that manages a set of
   * [diagnostics](#Diagnostic). Diagnostics are always scopes to a
   * diagnostics collection and a resource.
   *
   * To get an instance of a `DiagnosticCollection` use
   * [createDiagnosticCollection](#languages.createDiagnosticCollection).
   */
  export interface DiagnosticCollection {

    /**
     * The name of this diagnostic collection, for instance `typescript`. Every diagnostic
     * from this collection will be associated with this name. Also, the task framework uses this
     * name when defining [problem matchers](https://code.visualstudio.com/docs/editor/tasks#_defining-a-problem-matcher).
     */
    readonly name: string

    /**
     * Assign diagnostics for given resource. Will replace
     * existing diagnostics for that resource.
     *
     * @param uri A resource identifier.
     * @param diagnostics Array of diagnostics or `undefined`
     */
    set(uri: string, diagnostics: Diagnostic[] | null): void
    /**
     * Replace all entries in this collection.
     *
     * Diagnostics of multiple tuples of the same uri will be merged, e.g
     * `[[file1, [d1]], [file1, [d2]]]` is equivalent to `[[file1, [d1, d2]]]`.
     * If a diagnostics item is `undefined` as in `[file1, undefined]`
     * all previous but not subsequent diagnostics are removed.
     *
     * @param entries An array of tuples, like `[[file1, [d1, d2]], [file2, [d3, d4, d5]]]`, or `undefined`.
     */
    set(entries: [string, Diagnostic[] | null][] | string, diagnostics?: Diagnostic[]): void

    /**
     * Remove all diagnostics from this collection that belong
     * to the provided `uri`. The same as `#set(uri, undefined)`.
     *
     * @param uri A resource identifier.
     */
    delete(uri: string): void

    /**
     * Remove all diagnostics from this collection. The same
     * as calling `#set(undefined)`
     */
    clear(): void

    /**
     * Iterate over each entry in this collection.
     *
     * @param callback Function to execute for each entry.
     * @param thisArg The `this` context used when invoking the handler function.
     */
    forEach(callback: (uri: string, diagnostics: Diagnostic[], collection: DiagnosticCollection) => any, thisArg?: any): void

    /**
     * Get the diagnostics for a given resource. *Note* that you cannot
     * modify the diagnostics-array returned from this call.
     *
     * @param uri A resource identifier.
     * @returns An immutable array of [diagnostics](#Diagnostic) or `undefined`.
     */
    get(uri: string): Diagnostic[] | undefined

    /**
     * Check if this collection contains diagnostics for a
     * given resource.
     *
     * @param uri A resource identifier.
     * @returns `true` if this collection has diagnostic for the given resource.
     */
    has(uri: string): boolean

    /**
     * Dispose and free associated resources. Calls
     * [clear](#DiagnosticCollection.clear).
     */
    dispose(): void
  }

  export interface DiagnosticEventParams {
    bufnr: number
    uri: string
    diagnostics: ReadonlyArray<Diagnostic>
  }

  export namespace diagnosticManager {

    export const onDidRefresh: Event<DiagnosticEventParams>
    /**
     * Create collection by name
     */
    export function create(name: string): DiagnosticCollection

    /**
     * Get readonly diagnostics for uri
     */
    export function getDiagnostics(uri: string): { [collection: string]: Diagnostic[] }

    /**
     * Get readonly diagnostics by document and range.
     */
    export function getDiagnosticsInRange(doc: TextDocumentIdentifier, range: Range): ReadonlyArray<Diagnostic>
    /**
     * Get all sorted diagnostics
     */
    export function getDiagnosticList(): Promise<ReadonlyArray<DiagnosticItem>>

    /**
     * All diagnostics at current cursor position.
     */
    export function getCurrentDiagnostics(): Promise<ReadonlyArray<Diagnostic>>

    /**
     * Get diagnostic collection.
     */
    export function getCollectionByName(name: string): DiagnosticCollection
  }
  // }}

  // language client {{
  /**
   * An action to be performed when the connection is producing errors.
   */
  export enum ErrorAction {
    /**
     * Continue running the server.
     */
    Continue = 1,
    /**
     * Shutdown the server.
     */
    Shutdown = 2
  }
  /**
   * An action to be performed when the connection to a server got closed.
   */
  export enum CloseAction {
    /**
     * Don't restart the server. The connection stays closed.
     */
    DoNotRestart = 1,
    /**
     * Restart the server.
     */
    Restart = 2
  }
  /**
   * A pluggable error handler that is invoked when the connection is either
   * producing errors or got closed.
   */
  export interface ErrorHandler {
    /**
     * An error has occurred while writing or reading from the connection.
     *
     * @param error - the error received
     * @param message - the message to be delivered to the server if know.
     * @param count - a count indicating how often an error is received. Will
     *  be reset if a message got successfully send or received.
     */
    error(error: Error, message: { jsonrpc: string }, count: number): ErrorAction
    /**
     * The connection to the server got closed.
     */
    closed(): CloseAction
  }
  export interface InitializationFailedHandler {
    (error: Error | any): boolean
  }

  export interface SynchronizeOptions {
    configurationSection?: string | string[]
    fileEvents?: FileSystemWatcher | FileSystemWatcher[]
  }

  export enum RevealOutputChannelOn {
    Info = 1,
    Warn = 2,
    Error = 3,
    Never = 4
  }
  export interface ConfigurationItem {
    /**
     * The scope to get the configuration section for.
     */
    scopeUri?: string
    /**
     * The configuration section asked for.
     */
    section?: string
  }
  export interface ResponseError<D> {
    code: number
    data: D | undefined
  }

  export type HandlerResult<R, E> = R | ResponseError<E> | Thenable<R> | Thenable<ResponseError<E>> | Thenable<R | ResponseError<E>>

  export interface RequestHandler<P, R, E> {
    (params: P, token: CancellationToken): HandlerResult<R, E>
  }

  export interface RequestHandler0<R, E> {
    (token: CancellationToken): HandlerResult<R, E>
  }
  /**
   * The parameters of a configuration request.
   */
  export interface ConfigurationParams {
    items: ConfigurationItem[]
  }

  export interface ConfigurationWorkspaceMiddleware {
    configuration?: (params: ConfigurationParams, token: CancellationToken, next: RequestHandler<ConfigurationParams, any[], void>) => HandlerResult<any[], void>
  }

  export interface WorkspaceFolderWorkspaceMiddleware {
    workspaceFolders?: (token: CancellationToken, next: RequestHandler0<WorkspaceFolder[] | null, void>) => HandlerResult<WorkspaceFolder[] | null, void>
    didChangeWorkspaceFolders?: NextSignature<WorkspaceFoldersChangeEvent, void>
  }

  export interface ProvideTypeDefinitionSignature {
    (
      this: void,
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken
    ): ProviderResult<Definition | DefinitionLink[]>
  }

  export interface TypeDefinitionMiddleware {
    provideTypeDefinition?: (
      this: void,
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken,
      next: ProvideTypeDefinitionSignature
    ) => ProviderResult<Definition | DefinitionLink[]>
  }

  export interface ProvideImplementationSignature {
    (this: void, document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<Definition | DefinitionLink[]>
  }

  export interface ImplementationMiddleware {
    provideImplementation?: (this: void, document: LinesTextDocument, position: Position, token: CancellationToken, next: ProvideImplementationSignature) => ProviderResult<Definition | DefinitionLink[]>
  }
  export type ProvideDocumentColorsSignature = (document: LinesTextDocument, token: CancellationToken) => ProviderResult<ColorInformation[]>

  export type ProvideColorPresentationSignature = (
    color: Color,
    context: { document: LinesTextDocument; range: Range },
    token: CancellationToken
  ) => ProviderResult<ColorPresentation[]>

  export interface ColorProviderMiddleware {
    provideDocumentColors?: (
      this: void,
      document: LinesTextDocument,
      token: CancellationToken,
      next: ProvideDocumentColorsSignature
    ) => ProviderResult<ColorInformation[]>
    provideColorPresentations?: (
      this: void,
      color: Color,
      context: { document: LinesTextDocument; range: Range },
      token: CancellationToken,
      next: ProvideColorPresentationSignature
    ) => ProviderResult<ColorPresentation[]>
  }

  export interface ProvideDeclarationSignature {
    (this: void, document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<Declaration | DeclarationLink[]>
  }

  export interface DeclarationMiddleware {
    provideDeclaration?: (this: void, document: LinesTextDocument, position: Position, token: CancellationToken, next: ProvideDeclarationSignature) => ProviderResult<Declaration | DeclarationLink[]>
  }

  export type ProvideFoldingRangeSignature = (
    this: void,
    document: LinesTextDocument,
    context: FoldingContext,
    token: CancellationToken
  ) => ProviderResult<FoldingRange[]>

  export interface FoldingRangeProviderMiddleware {
    provideFoldingRanges?: (
      this: void,
      document: LinesTextDocument,
      context: FoldingContext,
      token: CancellationToken,
      next: ProvideFoldingRangeSignature
    ) => ProviderResult<FoldingRange[]>
  }

  export interface PrepareCallHierarchySignature {
    (this: void, document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<CallHierarchyItem | CallHierarchyItem[]>
  }

  export interface CallHierarchyIncomingCallsSignature {
    (this: void, item: CallHierarchyItem, token: CancellationToken): ProviderResult<CallHierarchyIncomingCall[]>
  }

  export interface CallHierarchyOutgoingCallsSignature {
    (this: void, item: CallHierarchyItem, token: CancellationToken): ProviderResult<CallHierarchyOutgoingCall[]>
  }
  export interface CallHierarchyMiddleware {
    prepareCallHierarchy?: (
      this: void,
      document: LinesTextDocument,
      positions: Position,
      token: CancellationToken,
      next: PrepareCallHierarchySignature
    ) => ProviderResult<CallHierarchyItem | CallHierarchyItem[]>
    provideCallHierarchyIncomingCalls?: (
      this: void,
      item: CallHierarchyItem,
      token: CancellationToken,
      next: CallHierarchyIncomingCallsSignature
    ) => ProviderResult<CallHierarchyIncomingCall[]>
    provideCallHierarchyOutgoingCalls?: (
      this: void,
      item: CallHierarchyItem,
      token: CancellationToken,
      next: CallHierarchyOutgoingCallsSignature
    ) => ProviderResult<CallHierarchyOutgoingCall[]>
  }

  export interface DocumentSemanticsTokensSignature {
    (this: void, document: LinesTextDocument, token: CancellationToken): ProviderResult<SemanticTokens>
  }

  export interface DocumentSemanticsTokensEditsSignature {
    (this: void, document: LinesTextDocument, previousResultId: string, token: CancellationToken): ProviderResult<SemanticTokens | SemanticTokensDelta>
  }

  export interface DocumentRangeSemanticTokensSignature {
    (this: void, document: LinesTextDocument, range: Range, token: CancellationToken): ProviderResult<SemanticTokens>
  }

  export interface SemanticTokensMiddleware {
    provideDocumentSemanticTokens?: (
      this: void,
      document: LinesTextDocument,
      token: CancellationToken,
      next: DocumentSemanticsTokensSignature
    ) => ProviderResult<SemanticTokens>
    provideDocumentSemanticTokensEdits?: (
      this: void,
      document: LinesTextDocument,
      previousResultId: string,
      token: CancellationToken,
      next: DocumentSemanticsTokensEditsSignature
    ) => ProviderResult<SemanticTokens | SemanticTokensDelta>
    provideDocumentRangeSemanticTokens?: (
      this: void,
      document: LinesTextDocument,
      range: Range,
      token: CancellationToken,
      next: DocumentRangeSemanticTokensSignature
    ) => ProviderResult<SemanticTokens>
  }


  /**
   * File operation middleware
   * @since 3.16.0
   */
  export interface FileOperationsMiddleware {
    didCreateFiles?: NextSignature<FileCreateEvent, void>
    willCreateFiles?: NextSignature<FileWillCreateEvent, Thenable<WorkspaceEdit | null | undefined>>
    didRenameFiles?: NextSignature<FileRenameEvent, void>
    willRenameFiles?: NextSignature<FileWillRenameEvent, Thenable<WorkspaceEdit | null | undefined>>
    didDeleteFiles?: NextSignature<FileDeleteEvent, void>
    willDeleteFiles?: NextSignature<FileWillDeleteEvent, Thenable<WorkspaceEdit | null | undefined>>
  }

  export interface ProvideLinkedEditingRangeSignature {
    (this: void, document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<LinkedEditingRanges>
  }

  export interface LinkedEditingRangeMiddleware {
    provideLinkedEditingRange?: (
      this: void,
      document: LinesTextDocument,
      position: Position,
      token: CancellationToken,
      next: ProvideLinkedEditingRangeSignature
    ) => ProviderResult<LinkedEditingRanges>
  }

  export interface ProvideSelectionRangeSignature {
    (this: void, document: LinesTextDocument, positions: Position[], token: CancellationToken): ProviderResult<SelectionRange[]>
  }

  export interface SelectionRangeProviderMiddleware {
    provideSelectionRanges?: (this: void, document: LinesTextDocument, positions: Position[], token: CancellationToken, next: ProvideSelectionRangeSignature) => ProviderResult<SelectionRange[]>
  }

  export interface HandleWorkDoneProgressSignature {
    (this: void, token: ProgressToken, params: WorkDoneProgressBegin | WorkDoneProgressReport | WorkDoneProgressEnd): void
  }

  export interface HandleDiagnosticsSignature {
    (this: void, uri: string, diagnostics: Diagnostic[]): void
  }

  export interface ProvideCompletionItemsSignature {
    (this: void, document: LinesTextDocument, position: Position, context: CompletionContext, token: CancellationToken): ProviderResult<CompletionItem[] | CompletionList | null>
  }

  export interface ResolveCompletionItemSignature {
    (this: void, item: CompletionItem, token: CancellationToken): ProviderResult<CompletionItem>
  }

  export interface ProvideHoverSignature {
    (this: void, document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<Hover>
  }

  export interface ProvideSignatureHelpSignature {
    (this: void, document: LinesTextDocument, position: Position, context: SignatureHelpContext, token: CancellationToken): ProviderResult<SignatureHelp>
  }

  export interface ProvideDefinitionSignature {
    (this: void, document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<Definition | DefinitionLink[]>
  }

  export interface ProvideReferencesSignature {
    (this: void, document: LinesTextDocument, position: Position, options: {
      includeDeclaration: boolean
    }, token: CancellationToken): ProviderResult<Location[]>
  }

  export interface ProvideDocumentHighlightsSignature {
    (this: void, document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<DocumentHighlight[]>
  }

  export interface ProvideDocumentSymbolsSignature {
    (this: void, document: LinesTextDocument, token: CancellationToken): ProviderResult<SymbolInformation[] | DocumentSymbol[]>
  }

  export interface ProvideWorkspaceSymbolsSignature {
    (this: void, query: string, token: CancellationToken): ProviderResult<SymbolInformation[]>
  }

  export interface ProvideCodeActionsSignature {
    (this: void, document: LinesTextDocument, range: Range, context: CodeActionContext, token: CancellationToken): ProviderResult<(Command | CodeAction)[]>
  }

  export interface ResolveCodeActionSignature {
    (this: void, item: CodeAction, token: CancellationToken): ProviderResult<CodeAction>
  }

  export interface ProvideCodeLensesSignature {
    (this: void, document: LinesTextDocument, token: CancellationToken): ProviderResult<CodeLens[]>
  }

  export interface ResolveCodeLensSignature {
    (this: void, codeLens: CodeLens, token: CancellationToken): ProviderResult<CodeLens>
  }

  export interface ProvideDocumentFormattingEditsSignature {
    (this: void, document: LinesTextDocument, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]>
  }

  export interface ProvideDocumentRangeFormattingEditsSignature {
    (this: void, document: LinesTextDocument, range: Range, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]>
  }

  export interface ProvideOnTypeFormattingEditsSignature {
    (this: void, document: LinesTextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]>
  }

  export interface PrepareRenameSignature {
    (this: void, document: LinesTextDocument, position: Position, token: CancellationToken): ProviderResult<Range | {
      range: Range
      placeholder: string
    }>
  }

  export interface ProvideRenameEditsSignature {
    (this: void, document: LinesTextDocument, position: Position, newName: string, token: CancellationToken): ProviderResult<WorkspaceEdit>
  }

  export interface ProvideDocumentLinksSignature {
    (this: void, document: LinesTextDocument, token: CancellationToken): ProviderResult<DocumentLink[]>
  }

  export interface ResolveDocumentLinkSignature {
    (this: void, link: DocumentLink, token: CancellationToken): ProviderResult<DocumentLink>
  }

  export interface ExecuteCommandSignature {
    (this: void, command: string, args: any[]): ProviderResult<any>
  }

  export interface NextSignature<P, R> {
    (this: void, data: P, next: (data: P) => R): R
  }

  export interface DidChangeConfigurationSignature {
    (this: void, sections: string[] | undefined): void
  }

  export interface DidChangeWatchedFileSignature {
    (this: void, event: FileEvent): void
  }

  export interface _WorkspaceMiddleware {
    didChangeConfiguration?: (this: void, sections: string[] | undefined, next: DidChangeConfigurationSignature) => void
    didChangeWatchedFile?: (this: void, event: FileEvent, next: DidChangeWatchedFileSignature) => void
  }

  export type WorkspaceMiddleware = _WorkspaceMiddleware & ConfigurationWorkspaceMiddleware & WorkspaceFolderWorkspaceMiddleware & FileOperationsMiddleware

  /**
   * Params to show a document.
   *
   * @since 3.16.0
   */
  export interface ShowDocumentParams {
    /**
       * The document uri to show.
    */
    uri: string
    /**
       * Indicates to show the resource in an external program.
       * To show for example `https://code.visualstudio.com/`
       * in the default WEB browser set `external` to `true`.
    */
    external?: boolean
    /**
       * An optional property to indicate whether the editor
       * showing the document should take focus or not.
       * Clients might ignore this property if an external
       * program in started.
    */
    takeFocus?: boolean
    /**
       * An optional selection range if the document is a text
       * document. Clients might ignore the property if an
       * external program is started or the file is not a text
       * file.
    */
    selection?: Range
  }
  /**
   * The result of an show document request.
   *
   * @since 3.16.0
   */
  export interface ShowDocumentResult {
    /**
     * A boolean indicating if the show was successful.
    */
    success: boolean
  }

  export interface _WindowMiddleware {
    showDocument?: (
      this: void,
      params: ShowDocumentParams,
      next: RequestHandler<ShowDocumentParams, ShowDocumentResult, void>
    ) => Promise<ShowDocumentResult>
  }
  export type WindowMiddleware = _WindowMiddleware

  /**
   * The Middleware lets extensions intercept the request and notifications send and received
   * from the server
   */
  interface _Middleware {
    didOpen?: NextSignature<LinesTextDocument, void>
    didChange?: NextSignature<DidChangeTextDocumentParams, void>
    willSave?: NextSignature<TextDocumentWillSaveEvent, void>
    willSaveWaitUntil?: NextSignature<TextDocumentWillSaveEvent, Thenable<TextEdit[]>>
    didSave?: NextSignature<LinesTextDocument, void>
    didClose?: NextSignature<LinesTextDocument, void>
    handleDiagnostics?: (this: void, uri: string, diagnostics: Diagnostic[], next: HandleDiagnosticsSignature) => void
    provideCompletionItem?: (this: void, document: LinesTextDocument, position: Position, context: CompletionContext, token: CancellationToken, next: ProvideCompletionItemsSignature) => ProviderResult<CompletionItem[] | CompletionList | null>
    resolveCompletionItem?: (this: void, item: CompletionItem, token: CancellationToken, next: ResolveCompletionItemSignature) => ProviderResult<CompletionItem>
    provideHover?: (this: void, document: LinesTextDocument, position: Position, token: CancellationToken, next: ProvideHoverSignature) => ProviderResult<Hover>
    provideSignatureHelp?: (this: void, document: LinesTextDocument, position: Position, context: SignatureHelpContext, token: CancellationToken, next: ProvideSignatureHelpSignature) => ProviderResult<SignatureHelp>
    provideDefinition?: (this: void, document: LinesTextDocument, position: Position, token: CancellationToken, next: ProvideDefinitionSignature) => ProviderResult<Definition | DefinitionLink[]>
    provideReferences?: (this: void, document: LinesTextDocument, position: Position, options: {
      includeDeclaration: boolean
    }, token: CancellationToken, next: ProvideReferencesSignature) => ProviderResult<Location[]>
    provideDocumentHighlights?: (this: void, document: LinesTextDocument, position: Position, token: CancellationToken, next: ProvideDocumentHighlightsSignature) => ProviderResult<DocumentHighlight[]>
    provideDocumentSymbols?: (this: void, document: LinesTextDocument, token: CancellationToken, next: ProvideDocumentSymbolsSignature) => ProviderResult<SymbolInformation[] | DocumentSymbol[]>
    provideWorkspaceSymbols?: (this: void, query: string, token: CancellationToken, next: ProvideWorkspaceSymbolsSignature) => ProviderResult<SymbolInformation[]>
    provideCodeActions?: (this: void, document: LinesTextDocument, range: Range, context: CodeActionContext, token: CancellationToken, next: ProvideCodeActionsSignature) => ProviderResult<(Command | CodeAction)[]>
    handleWorkDoneProgress?: (this: void, token: ProgressToken, params: WorkDoneProgressBegin | WorkDoneProgressReport | WorkDoneProgressEnd, next: HandleWorkDoneProgressSignature) => void
    resolveCodeAction?: (this: void, item: CodeAction, token: CancellationToken, next: ResolveCodeActionSignature) => ProviderResult<CodeAction>
    provideCodeLenses?: (this: void, document: LinesTextDocument, token: CancellationToken, next: ProvideCodeLensesSignature) => ProviderResult<CodeLens[]>
    resolveCodeLens?: (this: void, codeLens: CodeLens, token: CancellationToken, next: ResolveCodeLensSignature) => ProviderResult<CodeLens>
    provideDocumentFormattingEdits?: (this: void, document: LinesTextDocument, options: FormattingOptions, token: CancellationToken, next: ProvideDocumentFormattingEditsSignature) => ProviderResult<TextEdit[]>
    provideDocumentRangeFormattingEdits?: (this: void, document: LinesTextDocument, range: Range, options: FormattingOptions, token: CancellationToken, next: ProvideDocumentRangeFormattingEditsSignature) => ProviderResult<TextEdit[]>
    provideOnTypeFormattingEdits?: (this: void, document: LinesTextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken, next: ProvideOnTypeFormattingEditsSignature) => ProviderResult<TextEdit[]>
    prepareRename?: (this: void, document: LinesTextDocument, position: Position, token: CancellationToken, next: PrepareRenameSignature) => ProviderResult<Range | {
      range: Range
      placeholder: string
    }>
    provideRenameEdits?: (this: void, document: LinesTextDocument, position: Position, newName: string, token: CancellationToken, next: ProvideRenameEditsSignature) => ProviderResult<WorkspaceEdit>
    provideDocumentLinks?: (this: void, document: LinesTextDocument, token: CancellationToken, next: ProvideDocumentLinksSignature) => ProviderResult<DocumentLink[]>
    resolveDocumentLink?: (this: void, link: DocumentLink, token: CancellationToken, next: ResolveDocumentLinkSignature) => ProviderResult<DocumentLink>
    executeCommand?: (this: void, command: string, args: any[], next: ExecuteCommandSignature) => ProviderResult<any>
    workspace?: WorkspaceMiddleware
    window?: WindowMiddleware
  }
  export type Middleware = _Middleware & TypeDefinitionMiddleware & ImplementationMiddleware & ColorProviderMiddleware & DeclarationMiddleware & FoldingRangeProviderMiddleware & CallHierarchyMiddleware & SemanticTokensMiddleware & LinkedEditingRangeMiddleware & SelectionRangeProviderMiddleware

  export interface ConnectionOptions {
    // cancellationStrategy: CancellationStrategy
    maxRestartCount?: number
  }

  export interface LanguageClientOptions {
    ignoredRootPaths?: string[]
    disableSnippetCompletion?: boolean
    disableDynamicRegister?: boolean
    disabledFeatures?: string[]
    formatterPriority?: number
    documentSelector?: DocumentSelector | string[]
    synchronize?: SynchronizeOptions
    diagnosticCollectionName?: string
    outputChannelName?: string
    outputChannel?: OutputChannel
    revealOutputChannelOn?: RevealOutputChannelOn
    /**
     * The encoding use to read stdout and stderr. Defaults
     * to 'utf8' if omitted.
     */
    stdioEncoding?: string
    initializationOptions?: any | (() => any)
    initializationFailedHandler?: InitializationFailedHandler
    progressOnInitialization?: boolean
    errorHandler?: ErrorHandler
    middleware?: Middleware
    workspaceFolder?: WorkspaceFolder
    connectionOptions?: ConnectionOptions
    markdown?: {
      isTrusted: boolean
    }
  }
  export enum State {
    Stopped = 1,
    Running = 2,
    Starting = 3
  }
  export interface StateChangeEvent {
    oldState: State
    newState: State
  }
  export enum ClientState {
    Initial = 0,
    Starting = 1,
    StartFailed = 2,
    Running = 3,
    Stopping = 4,
    Stopped = 5
  }
  export interface RegistrationData<T> {
    id: string
    registerOptions: T
  }
  /**
   * A static feature. A static feature can't be dynamically activate via the
   * server. It is wired during the initialize sequence.
   */
  export interface StaticFeature {
    /**
     * Called to fill the initialize params.
     *
     * @params the initialize params.
     */
    fillInitializeParams?: (params: any) => void
    /**
     * Called to fill in the client capabilities this feature implements.
     *
     * @param capabilities The client capabilities to fill.
     */
    fillClientCapabilities(capabilities: any): void
    /**
     * Initialize the feature. This method is called on a feature instance
     * when the client has successfully received the initialize request from
     * the server and before the client sends the initialized notification
     * to the server.
     *
     * @param capabilities the server capabilities
     * @param documentSelector the document selector pass to the client's constructor.
     *  May be `undefined` if the client was created without a selector.
     */
    initialize(capabilities: any, documentSelector: DocumentSelector | undefined): void
    /**
     * Called when the client is stopped to dispose this feature. Usually a feature
     * unregisters listeners registered hooked up with the VS Code extension host.
     */
    dispose(): void
  }

  class ParameterStructures {
    private readonly kind
    /**
     * The parameter structure is automatically inferred on the number of parameters
     * and the parameter type in case of a single param.
    */
    static readonly auto: ParameterStructures
    /**
     * Forces `byPosition` parameter structure. This is useful if you have a single
     * parameter which has a literal type.
    */
    static readonly byPosition: ParameterStructures
    /**
     * Forces `byName` parameter structure. This is only useful when having a single
     * parameter. The library will report errors if used with a different number of
     * parameters.
    */
    static readonly byName: ParameterStructures
    private constructor()
    static is(value: any): value is ParameterStructures
    toString(): string
  }
  /**
   * An interface to type messages.
   */
  export interface MessageSignature {
    readonly method: string
    readonly numberOfParams: number
    readonly parameterStructures: ParameterStructures
  }

  /**
   *
   * An abstract implementation of a MessageType.
   */
  abstract class AbstractMessageSignature implements MessageSignature {
    readonly method: string
    readonly numberOfParams: number
    constructor(method: string, numberOfParams: number)
    get parameterStructures(): ParameterStructures
  }

  /**
   * Classes to type request response pairs
   */
  export class RequestType0<R, E> extends AbstractMessageSignature {
    /**
     * Clients must not use this property. It is here to ensure correct typing.
     */
    readonly _: [R, E, _EM] | undefined
    constructor(method: string)
  }

  export class RequestType<P, R, E> extends AbstractMessageSignature {
    private _parameterStructures
    /**
     * Clients must not use this property. It is here to ensure correct typing.
     */
    readonly _: [P, R, E, _EM] | undefined
    constructor(method: string, _parameterStructures?: ParameterStructures)
    get parameterStructures(): ParameterStructures
  }

  export class NotificationType<P> extends AbstractMessageSignature {
    /**
     * Clients must not use this property. It is here to ensure correct typing.
     */
    readonly _: [P, _EM] | undefined
    constructor(method: string)
  }

  export class NotificationType0 extends AbstractMessageSignature {
    /**
     * Clients must not use this property. It is here to ensure correct typing.
     */
    readonly _: [_EM] | undefined
    constructor(method: string)
  }

  export interface InitializeParams {
    /**
     * The process Id of the parent process that started
     * the server.
     */
    processId: number | null
    /**
     * Information about the client
     *
     * @since 3.15.0
     */
    clientInfo?: {
      /**
       * The name of the client as defined by the client.
       */
      name: string
      /**
       * The client's version as defined by the client.
       */
      version?: string
    }
    /**
     * The rootPath of the workspace. Is null
     * if no folder is open.
     *
     * @deprecated in favour of rootUri.
     */
    rootPath?: string | null
    /**
     * The rootUri of the workspace. Is null if no
     * folder is open. If both `rootPath` and `rootUri` are set
     * `rootUri` wins.
     *
     * @deprecated in favour of workspaceFolders.
     */
    rootUri: string | null
    /**
     * The capabilities provided by the client (editor or tool)
     */
    capabilities: any
    /**
     * User provided initialization options.
     */
    initializationOptions?: any
    /**
     * The initial trace setting. If omitted trace is disabled ('off').
     */
    trace?: 'off' | 'messages' | 'verbose'
    /**
     * An optional token that a server can use to report work done progress.
     */
    workDoneToken?: ProgressToken
  }
  class RegistrationType<RO> {
    /**
     * Clients must not use this property. It is here to ensure correct typing.
     */
    readonly ____: [RO, _EM] | undefined
    readonly method: string
    constructor(method: string)
  }
  /**
   * The result returned from an initialize request.
   */
  export interface InitializeResult {
    /**
     * The capabilities the language server provides.
     */
    capabilities: any
    /**
     * Information about the server.
     *
     * @since 3.15.0
     */
    serverInfo?: {
      /**
       * The name of the server as defined by the server.
       */
      name: string
      /**
       * The servers's version as defined by the server.
       */
      version?: string
    }
    /**
     * Custom initialization results.
     */
    [custom: string]: any
  }

  export interface DynamicFeature<RO> {
    /**
     * Called to fill the initialize params.
     *
     * @params the initialize params.
     */
    fillInitializeParams?: (params: InitializeParams) => void
    /**
     * Called to fill in the client capabilities this feature implements.
     *
     * @param capabilities The client capabilities to fill.
     */
    fillClientCapabilities(capabilities: any): void
    /**
     * Initialize the feature. This method is called on a feature instance
     * when the client has successfully received the initialize request from
     * the server and before the client sends the initialized notification
     * to the server.
     *
     * @param capabilities the server capabilities.
     * @param documentSelector the document selector pass to the client's constructor.
     *  May be `undefined` if the client was created without a selector.
     */
    initialize(capabilities: any, documentSelector: DocumentSelector | undefined): void
    /**
      * The signature (e.g. method) for which this features support dynamic activation / registration.
      */
    registrationType: RegistrationType<RO>
    /**
     * Is called when the server send a register request for the given message.
     *
     * @param data additional registration data as defined in the protocol.
     */
    register(data: RegistrationData<RO>): void
    /**
     * Is called when the server wants to unregister a feature.
     *
     * @param id the id used when registering the feature.
     */
    unregister(id: string): void
    /**
     * Called when the client is stopped to dispose this feature. Usually a feature
     * unregisters listeners registered hooked up with the VS Code extension host.
     */
    dispose(): void
  }

  export interface NotificationFeature<T extends Function> {
    /**
     * Triggers the corresponding RPC method.
     */
    getProvider(document: TextDocument): {
      send: T
    }
  }

  export interface ExecutableOptions {
    cwd?: string
    env?: any
    detached?: boolean
    shell?: boolean
  }

  export interface Executable {
    command: string
    args?: string[]
    options?: ExecutableOptions
  }

  export interface ForkOptions {
    cwd?: string
    env?: any
    execPath?: string
    encoding?: string
    execArgv?: string[]
  }

  export interface StreamInfo {
    writer: NodeJS.WritableStream
    reader: NodeJS.ReadableStream
    detached?: boolean
  }

  export enum TransportKind {
    stdio = 0,
    ipc = 1,
    pipe = 2,
    socket = 3
  }

  export interface SocketTransport {
    kind: TransportKind.socket
    port: number
  }

  export interface NodeModule {
    module: string
    transport?: TransportKind | SocketTransport
    args?: string[]
    runtime?: string
    options?: ForkOptions
  }

  export interface ChildProcessInfo {
    process: cp.ChildProcess
    detached: boolean
  }

  export interface PartialMessageInfo {
    readonly messageToken: number
    readonly waitingTime: number
  }

  export interface MessageReader {
    readonly onError: Event<Error>
    readonly onClose: Event<void>
    readonly onPartialMessage: Event<PartialMessageInfo>
    listen(callback: (data: { jsonrpc: string }) => void): void
    dispose(): void
  }

  export interface MessageWriter {
    readonly onError: Event<[Error, { jsonrpc: string } | undefined, number | undefined]>
    readonly onClose: Event<void>
    write(msg: { jsonrpc: string }): void
    dispose(): void
  }

  export class NullLogger {
    constructor()
    error(message: string): void
    warn(message: string): void
    info(message: string): void
    log(message: string): void
  }

  export interface MessageTransports {
    reader: MessageReader
    writer: MessageWriter
    detached?: boolean
  }

  export namespace MessageTransports {
    /**
    * Checks whether the given value conforms to the [MessageTransports](#MessageTransports) interface.
    */
    function is(value: any): value is MessageTransports
  }

  export type ServerOptions = Executable | NodeModule | {
    run: Executable
    debug: Executable
  } | {
    run: NodeModule
    debug: NodeModule
  } | (() => Promise<cp.ChildProcess | StreamInfo | MessageTransports | ChildProcessInfo>)

  export interface _EM {
    _$endMarker$_: number
  }

  export class ProgressType<P> {
    /**
     * Clients must not use this property. It is here to ensure correct typing.
     */
    readonly __?: [P, _EM]
    constructor()
  }

  export enum Trace {
    Off = 0,
    Messages = 1,
    Verbose = 2
  }

  /**
   * A language server for manage a language server.
   * It's recommended to use `services.registLanguageClient` for regist language client to serviers,
   * you can have language client listed in `CocList services` and services could start the language client
   * by `documentselector` of `clientOptions`.
   */
  export class LanguageClient {
    readonly id: string
    readonly name: string
    constructor(id: string, name: string, serverOptions: ServerOptions, clientOptions: LanguageClientOptions, forceDebug?: boolean)
    /**
     * Create language client by name and options, don't forget regist language client
     * to services by `services.registLanguageClient`
     */
    constructor(name: string, serverOptions: ServerOptions, clientOptions: LanguageClientOptions, forceDebug?: boolean)
    /**
     * R => result
     * E => Error result
     */
    sendRequest<R, E>(type: RequestType0<R, E>, token?: CancellationToken): Promise<R>
    /**
     * P => params
     * R => result
     * E => Error result
     */
    sendRequest<P, R, E>(type: RequestType<P, R, E>, params: P, token?: CancellationToken): Promise<R>
    sendRequest<R>(method: string, token?: CancellationToken): Promise<R>
    sendRequest<R>(method: string, param: any, token?: CancellationToken): Promise<R>

    onRequest<R, E>(type: RequestType0<R, E>, handler: RequestHandler0<R, E>): Disposable
    onRequest<P, R, E>(type: RequestType<P, R, E>, handler: RequestHandler<P, R, E>): Disposable
    onRequest<R, E>(method: string, handler: (...params: any[]) => HandlerResult<R, E>): Disposable

    sendNotification(type: NotificationType0): void
    sendNotification<P>(type: NotificationType<P>, params?: P): void
    sendNotification(method: string): void
    sendNotification(method: string, params: any): void

    onNotification(type: NotificationType0, handler: () => void): Disposable
    onNotification<P>(type: NotificationType<P>, handler: (params: P) => void): Disposable
    onNotification(method: string, handler: (...params: any[]) => void): Disposable

    onProgress<P>(type: ProgressType<any>, token: string | number, handler: (params: P) => void): Disposable
    sendProgress<P>(type: ProgressType<P>, token: string | number, value: P): void

    /**
     * Append info to outputChannel
     */
    info(message: string, data?: any): void
    /**
     * Append warning to outputChannel
     */
    warn(message: string, data?: any): void
    /**
     * append error to outputChannel
     */
    error(message: string, data?: any): void
    getPublicState(): State
    get initializeResult(): InitializeResult | undefined

    get clientOptions(): LanguageClientOptions
    /**
     * Fired on language server state change.
     */
    get onDidChangeState(): Event<StateChangeEvent>
    get outputChannel(): OutputChannel
    get diagnostics(): DiagnosticCollection | undefined

    /**
     * Current running state.
     */
    get serviceState(): ServiceStat
    /**
     * Check if server could start.
     */
    needsStart(): boolean
    /**
     * Check if server could stop.
     */
    needsStop(): boolean
    onReady(): Promise<void>
    get started(): boolean
    set trace(value: Trace)

    /**
     * Stop language server.
     */
    stop(): Promise<void>

    /**
     * Start language server, not needed when registered to services by `services.registLanguageClient`
     */
    start(): Disposable
    /**
     * Restart language client.
     */
    restart(): void

    /**
     * Register custom feature.
     */
    registerFeature(feature: StaticFeature | DynamicFeature<any>): void

    /**
     * Log failed request to outputChannel.
     */
    handleFailedRequest<T>(type: MessageSignature, token: CancellationToken | undefined, error: any, defaultValue: T)
  }

  /**
   * Monitor for setting change, restart language server when specified setting changed.
   */
  export class SettingMonitor {
    constructor(client: LanguageClient, setting: string)
    start(): Disposable
  }
  // }}
}
// vim: set sw=2 ts=2 sts=2 et foldmarker={{,}} foldmethod=marker foldlevel=0 nofen:
