'use strict'
import { CallHierarchyIncomingCall, CallHierarchyItem, CallHierarchyOutgoingCall, CancellationToken, CodeAction, CodeActionContext, CodeActionKind, CodeLens, ColorInformation, ColorPresentation, DefinitionLink, Disposable, DocumentHighlight, DocumentLink, DocumentSelector, DocumentSymbol, Emitter, Event, FoldingRange, FormattingOptions, Hover, LinkedEditingRanges, Location, LocationLink, Position, Range, SelectionRange, SemanticTokens, SemanticTokensDelta, SemanticTokensLegend, SignatureHelp, SignatureHelpContext, SymbolInformation, TextEdit, WorkspaceEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import DiagnosticCollection from './diagnostic/collection'
import diagnosticManager from './diagnostic/manager'
import { CallHierarchyProvider, CodeActionProvider, CodeLensProvider, CompletionItemProvider, DeclarationProvider, DefinitionProvider, DocumentColorProvider, DocumentFormattingEditProvider, DocumentHighlightProvider, DocumentLinkProvider, DocumentRangeFormattingEditProvider, DocumentRangeSemanticTokensProvider, DocumentSemanticTokensProvider, DocumentSymbolProvider, DocumentSymbolProviderMetadata, FoldingContext, FoldingRangeProvider, HoverProvider, ImplementationProvider, InlayHintsProvider, LinkedEditingRangeProvider, OnTypeFormattingEditProvider, ReferenceContext, ReferenceProvider, RenameProvider, SelectionRangeProvider, SignatureHelpProvider, TypeDefinitionProvider, WorkspaceSymbolProvider } from './provider'
import CallHierarchyManager from './provider/callHierarchyManager'
import CodeActionManager from './provider/codeActionManager'
import CodeLensManager from './provider/codeLensManager'
import DeclarationManager from './provider/declarationManager'
import DefinitionManager from './provider/definitionManager'
import DocumentColorManager from './provider/documentColorManager'
import DocumentHighlightManager from './provider/documentHighlightManager'
import DocumentLinkManager from './provider/documentLinkManager'
import DocumentSymbolManager from './provider/documentSymbolManager'
import FoldingRangeManager from './provider/foldingRangeManager'
import FormatManager from './provider/formatManager'
import FormatRangeManager from './provider/formatRangeManager'
import HoverManager from './provider/hoverManager'
import ImplementationManager from './provider/implementationManager'
import LinkedEditingRangeManager from './provider/linkedEditingRangeManager'
import OnTypeFormatManager from './provider/onTypeFormatManager'
import ReferenceManager from './provider/referenceManager'
import RenameManager from './provider/renameManager'
import SelectionRangeManager from './provider/selectionRangeManager'
import SemanticTokensManager from './provider/semanticTokensManager'
import SemanticTokensRangeManager from './provider/semanticTokensRangeManager'
import SignatureManager from './provider/signatureManager'
import TypeDefinitionManager from './provider/typeDefinitionManager'
import WorkspaceSymbolManager from './provider/workspaceSymbolsManager'
import InlayHintManger, { InlayHintWithProvider } from './provider/inlayHintManager'
import { ExtendedCodeAction } from './types'
import { disposeAll } from './util'
const logger = require('./util/logger')('languages')

class Languages {
  private readonly _onDidSemanticTokensRefresh = new Emitter<DocumentSelector>()
  private readonly _onDidInlayHintRefresh = new Emitter<DocumentSelector>()
  public readonly onDidSemanticTokensRefresh: Event<DocumentSelector> = this._onDidSemanticTokensRefresh.event
  public readonly onDidInlayHintRefresh: Event<DocumentSelector> = this._onDidInlayHintRefresh.event
  private onTypeFormatManager = new OnTypeFormatManager()
  private documentLinkManager = new DocumentLinkManager()
  private documentColorManager = new DocumentColorManager()
  private foldingRangeManager = new FoldingRangeManager()
  private renameManager = new RenameManager()
  private formatManager = new FormatManager()
  private codeActionManager = new CodeActionManager()
  private workspaceSymbolsManager = new WorkspaceSymbolManager()
  private formatRangeManager = new FormatRangeManager()
  private hoverManager = new HoverManager()
  private signatureManager = new SignatureManager()
  private documentSymbolManager = new DocumentSymbolManager()
  private documentHighlightManager = new DocumentHighlightManager()
  private definitionManager = new DefinitionManager()
  private declarationManager = new DeclarationManager()
  private typeDefinitionManager = new TypeDefinitionManager()
  private referenceManager = new ReferenceManager()
  private implementationManager = new ImplementationManager()
  private codeLensManager = new CodeLensManager()
  private selectionRangeManager = new SelectionRangeManager()
  private callHierarchyManager = new CallHierarchyManager()
  private semanticTokensManager = new SemanticTokensManager()
  private semanticTokensRangeManager = new SemanticTokensRangeManager()
  private linkedEditingManager = new LinkedEditingRangeManager()
  private inlayHintManager = new InlayHintManger()

  public hasFormatProvider(doc: TextDocument): boolean {
    if (this.formatManager.hasProvider(doc)) {
      return true
    }
    if (this.formatRangeManager.hasProvider(doc)) {
      return true
    }
    return false
  }

  public registerOnTypeFormattingEditProvider(
    selector: DocumentSelector,
    provider: OnTypeFormattingEditProvider,
    triggerCharacters: string[]
  ): Disposable {
    return this.onTypeFormatManager.register(selector, provider, triggerCharacters)
  }

  public registerCompletionItemProvider(
    name: string,
    shortcut: string,
    selector: DocumentSelector | null,
    provider: CompletionItemProvider,
    triggerCharacters: string[] = [],
    priority?: number,
    allCommitCharacters?: string[]
  ): Disposable {
    selector = typeof selector == 'string' ? [{ language: selector }] : selector
    let sources = require('./sources/index').default
    return sources.createLanguageSource(name, shortcut, selector, provider, triggerCharacters, priority, allCommitCharacters)
  }

  public registerCodeActionProvider(selector: DocumentSelector, provider: CodeActionProvider, clientId: string | undefined, codeActionKinds?: CodeActionKind[]): Disposable {
    return this.codeActionManager.register(selector, provider, clientId, codeActionKinds)
  }

  public registerHoverProvider(selector: DocumentSelector, provider: HoverProvider): Disposable {
    return this.hoverManager.register(selector, provider)
  }

  public registerSelectionRangeProvider(selector: DocumentSelector, provider: SelectionRangeProvider): Disposable {
    return this.selectionRangeManager.register(selector, provider)
  }

  public registerSignatureHelpProvider(
    selector: DocumentSelector,
    provider: SignatureHelpProvider,
    triggerCharacters?: string[]): Disposable {
    return this.signatureManager.register(selector, provider, triggerCharacters)
  }

  public registerDocumentSymbolProvider(selector: DocumentSelector, provider: DocumentSymbolProvider, metadata?: DocumentSymbolProviderMetadata): Disposable {
    return this.documentSymbolManager.register(selector, provider, metadata)
  }

  public registerFoldingRangeProvider(selector: DocumentSelector, provider: FoldingRangeProvider): Disposable {
    return this.foldingRangeManager.register(selector, provider)
  }

  public registerDocumentHighlightProvider(selector: DocumentSelector, provider: DocumentHighlightProvider): Disposable {
    return this.documentHighlightManager.register(selector, provider)
  }

  public registerCodeLensProvider(selector: DocumentSelector, provider: CodeLensProvider): Disposable {
    return this.codeLensManager.register(selector, provider)
  }

  public registerDocumentLinkProvider(selector: DocumentSelector, provider: DocumentLinkProvider): Disposable {
    return this.documentLinkManager.register(selector, provider)
  }

  public registerDocumentColorProvider(selector: DocumentSelector, provider: DocumentColorProvider): Disposable {
    return this.documentColorManager.register(selector, provider)
  }

  public registerDefinitionProvider(selector: DocumentSelector, provider: DefinitionProvider): Disposable {
    return this.definitionManager.register(selector, provider)
  }

  public registerDeclarationProvider(selector: DocumentSelector, provider: DeclarationProvider): Disposable {
    return this.declarationManager.register(selector, provider)
  }

  public registerTypeDefinitionProvider(selector: DocumentSelector, provider: TypeDefinitionProvider): Disposable {
    return this.typeDefinitionManager.register(selector, provider)
  }

  public registerImplementationProvider(selector: DocumentSelector, provider: ImplementationProvider): Disposable {
    return this.implementationManager.register(selector, provider)
  }

  public registerReferencesProvider(selector: DocumentSelector, provider: ReferenceProvider): Disposable {
    return this.referenceManager.register(selector, provider)
  }

  public registerRenameProvider(selector: DocumentSelector, provider: RenameProvider): Disposable {
    return this.renameManager.register(selector, provider)
  }

  public registerWorkspaceSymbolProvider(provider: WorkspaceSymbolProvider): Disposable {
    if (arguments.length > 1 && typeof arguments[1].provideWorkspaceSymbols === 'function') {
      provider = arguments[1]
    }
    return this.workspaceSymbolsManager.register(provider)
  }

  public registerDocumentFormatProvider(selector: DocumentSelector, provider: DocumentFormattingEditProvider, priority = 0): Disposable {
    return this.formatManager.register(selector, provider, priority)
  }

  public registerDocumentRangeFormatProvider(selector: DocumentSelector, provider: DocumentRangeFormattingEditProvider, priority = 0): Disposable {
    return this.formatRangeManager.register(selector, provider, priority)
  }

  public registerCallHierarchyProvider(selector: DocumentSelector, provider: CallHierarchyProvider): Disposable {
    return this.callHierarchyManager.register(selector, provider)
  }

  public registerDocumentSemanticTokensProvider(selector: DocumentSelector, provider: DocumentSemanticTokensProvider, legend: SemanticTokensLegend): Disposable {
    // Language server may send refresh short time after initialized.
    let timer = setTimeout(() => {
      this._onDidSemanticTokensRefresh.fire(selector)
    }, 500)
    let disposable = this.semanticTokensManager.register(selector, provider, legend, () => {
      clearTimeout(timer)
      this._onDidSemanticTokensRefresh.fire(selector)
    })
    return Disposable.create(() => {
      clearTimeout(timer)
      disposable.dispose()
    })
  }

  public registerDocumentRangeSemanticTokensProvider(selector: DocumentSelector, provider: DocumentRangeSemanticTokensProvider, legend: SemanticTokensLegend): Disposable {
    this._onDidSemanticTokensRefresh.fire(selector)
    return this.semanticTokensRangeManager.register(selector, provider, legend)
  }

  public registerInlayHintsProvider(selector: DocumentSelector, provider: InlayHintsProvider): Disposable {
    let disposables: Disposable[] = []
    disposables.push(this.inlayHintManager.register(selector, provider))
    this._onDidInlayHintRefresh.fire(selector)
    if (typeof provider.onDidChangeInlayHints === 'function') {
      provider.onDidChangeInlayHints(() => {
        this._onDidInlayHintRefresh.fire(selector)
      }, null, disposables)
    }
    return Disposable.create(() => {
      disposeAll(disposables)
      this._onDidInlayHintRefresh.fire(selector)
    })
  }

  public registerLinkedEditingRangeProvider(selector: DocumentSelector, provider: LinkedEditingRangeProvider): Disposable {
    return this.linkedEditingManager.register(selector, provider)
  }

  public shouldTriggerSignatureHelp(document: TextDocument, triggerCharacter: string): boolean {
    return this.signatureManager.shouldTrigger(document, triggerCharacter)
  }

  public async getHover(document: TextDocument, position: Position, token: CancellationToken): Promise<Hover[]> {
    return await this.hoverManager.provideHover(document, position, token)
  }

  public async getSignatureHelp(document: TextDocument, position: Position, token: CancellationToken, context: SignatureHelpContext): Promise<SignatureHelp> {
    return await this.signatureManager.provideSignatureHelp(document, position, token, context)
  }

  public async getDefinition(document: TextDocument, position: Position, token: CancellationToken): Promise<Location[]> {
    if (!this.definitionManager.hasProvider(document)) return null
    return await this.definitionManager.provideDefinition(document, position, token)
  }

  public async getDefinitionLinks(document: TextDocument, position: Position, token: CancellationToken): Promise<DefinitionLink[]> {
    if (!this.definitionManager.hasProvider(document)) return null
    return await this.definitionManager.provideDefinitionLinks(document, position, token)
  }

  public async getDeclaration(document: TextDocument, position: Position, token: CancellationToken): Promise<Location[] | Location | LocationLink[] | null> {
    if (!this.declarationManager.hasProvider(document)) return null
    return await this.declarationManager.provideDeclaration(document, position, token)
  }

  public async getTypeDefinition(document: TextDocument, position: Position, token: CancellationToken): Promise<Location[]> {
    if (!this.typeDefinitionManager.hasProvider(document)) return null
    return await this.typeDefinitionManager.provideTypeDefinition(document, position, token)
  }

  public async getImplementation(document: TextDocument, position: Position, token: CancellationToken): Promise<Location[]> {
    if (!this.implementationManager.hasProvider(document)) return null
    return await this.implementationManager.provideReferences(document, position, token)
  }

  public async getReferences(document: TextDocument, context: ReferenceContext, position: Position, token: CancellationToken): Promise<Location[]> {
    if (!this.referenceManager.hasProvider(document)) return null
    return await this.referenceManager.provideReferences(document, position, context, token)
  }

  public async getDocumentSymbol(document: TextDocument, token: CancellationToken): Promise<SymbolInformation[] | DocumentSymbol[]> {
    return await this.documentSymbolManager.provideDocumentSymbols(document, token)
  }

  public getDocumentSymbolMetadata(document: TextDocument): DocumentSymbolProviderMetadata {
    return this.documentSymbolManager.getMetaData(document)
  }

  public async getSelectionRanges(document: TextDocument, positions: Position[], token): Promise<SelectionRange[] | null> {
    return await this.selectionRangeManager.provideSelectionRanges(document, positions, token)
  }

  public async getWorkspaceSymbols(query: string, token: CancellationToken): Promise<SymbolInformation[]> {
    query = query || ''
    return await this.workspaceSymbolsManager.provideWorkspaceSymbols(query, token)
  }

  public async resolveWorkspaceSymbol(symbol: SymbolInformation, token: CancellationToken): Promise<SymbolInformation> {
    return await this.workspaceSymbolsManager.resolveWorkspaceSymbol(symbol, token)
  }

  public async prepareRename(document: TextDocument, position: Position, token: CancellationToken): Promise<Range | { range: Range; placeholder: string } | false> {
    return await this.renameManager.prepareRename(document, position, token)
  }

  public async provideRenameEdits(document: TextDocument, position: Position, newName: string, token: CancellationToken): Promise<WorkspaceEdit> {
    return await this.renameManager.provideRenameEdits(document, position, newName, token)
  }

  public async provideDocumentFormattingEdits(document: TextDocument, options: FormattingOptions, token: CancellationToken): Promise<TextEdit[]> {
    if (!this.formatManager.hasProvider(document)) {
      let hasRangeFormatter = this.formatRangeManager.hasProvider(document)
      if (!hasRangeFormatter) return null
      let end = document.positionAt(document.getText().length)
      let range = Range.create(Position.create(0, 0), end)
      return await this.provideDocumentRangeFormattingEdits(document, range, options, token)
    }
    return await this.formatManager.provideDocumentFormattingEdits(document, options, token)
  }

  public async provideDocumentRangeFormattingEdits(document: TextDocument, range: Range, options: FormattingOptions, token: CancellationToken): Promise<TextEdit[]> {
    if (!this.formatRangeManager.hasProvider(document)) return null
    return await this.formatRangeManager.provideDocumentRangeFormattingEdits(document, range, options, token)
  }

  public async getCodeActions(document: TextDocument, range: Range, context: CodeActionContext, token: CancellationToken): Promise<ExtendedCodeAction[]> {
    return await this.codeActionManager.provideCodeActions(document, range, context, token)
  }

  public async getDocumentHighLight(document: TextDocument, position: Position, token: CancellationToken): Promise<DocumentHighlight[]> {
    return await this.documentHighlightManager.provideDocumentHighlights(document, position, token)
  }

  public async getDocumentLinks(document: TextDocument, token: CancellationToken): Promise<DocumentLink[]> {
    if (!this.documentLinkManager.hasProvider(document)) {
      return null
    }
    return (await this.documentLinkManager.provideDocumentLinks(document, token)) || []
  }

  public async resolveDocumentLink(link: DocumentLink, token: CancellationToken): Promise<DocumentLink> {
    return await this.documentLinkManager.resolveDocumentLink(link, token)
  }

  public async provideDocumentColors(document: TextDocument, token: CancellationToken): Promise<ColorInformation[] | null> {
    return await this.documentColorManager.provideDocumentColors(document, token)
  }

  public async provideFoldingRanges(document: TextDocument, context: FoldingContext, token: CancellationToken): Promise<FoldingRange[] | null> {
    if (!this.foldingRangeManager.hasProvider(document)) return null
    return await this.foldingRangeManager.provideFoldingRanges(document, context, token)
  }

  public async provideColorPresentations(color: ColorInformation, document: TextDocument, token: CancellationToken): Promise<ColorPresentation[]> {
    return await this.documentColorManager.provideColorPresentations(color, document, token)
  }

  public async getCodeLens(document: TextDocument, token: CancellationToken): Promise<(CodeLens | null)[]> {
    return await this.codeLensManager.provideCodeLenses(document, token)
  }

  public async resolveCodeLens(codeLens: CodeLens, token: CancellationToken): Promise<CodeLens> {
    if (codeLens.command != null) return codeLens
    return await this.codeLensManager.resolveCodeLens(codeLens, token)
  }

  public async resolveCodeAction(codeAction: ExtendedCodeAction, token: CancellationToken): Promise<CodeAction> {
    return await this.codeActionManager.resolveCodeAction(codeAction, token)
  }

  public async provideDocumentOnTypeEdits(
    character: string,
    document: TextDocument,
    position: Position,
    token: CancellationToken
  ): Promise<TextEdit[] | null> {
    return this.onTypeFormatManager.onCharacterType(character, document, position, token)
  }

  public canFormatOnType(character: string, document: TextDocument): boolean {
    return this.onTypeFormatManager.getProvider(document, character) != null
  }

  public async prepareCallHierarchy(document: TextDocument, position: Position, token: CancellationToken): Promise<CallHierarchyItem | CallHierarchyItem[]> {
    return this.callHierarchyManager.prepareCallHierarchy(document, position, token)
  }

  public async provideIncomingCalls(document: TextDocument, item: CallHierarchyItem, token: CancellationToken): Promise<CallHierarchyIncomingCall[]> {
    return this.callHierarchyManager.provideCallHierarchyIncomingCalls(document, item, token)
  }

  public async provideOutgoingCalls(document: TextDocument, item: CallHierarchyItem, token: CancellationToken): Promise<CallHierarchyOutgoingCall[]> {
    return this.callHierarchyManager.provideCallHierarchyOutgoingCalls(document, item, token)
  }

  public getLegend(document: TextDocument, range?: boolean): SemanticTokensLegend | undefined {
    if (range) return this.semanticTokensRangeManager.getLegend(document)
    return this.semanticTokensManager.getLegend(document)
  }

  public hasSemanticTokensEdits(document: TextDocument): boolean {
    return this.semanticTokensManager.hasSemanticTokensEdits(document)
  }

  public async provideDocumentSemanticTokens(document: TextDocument, token: CancellationToken): Promise<SemanticTokens> {
    return this.semanticTokensManager.provideDocumentSemanticTokens(document, token)
  }

  public async provideDocumentSemanticTokensEdits(document: TextDocument, previousResultId: string, token: CancellationToken): Promise<SemanticTokens | SemanticTokensDelta> {
    return this.semanticTokensManager.provideDocumentSemanticTokensEdits(document, previousResultId, token)
  }

  public async provideDocumentRangeSemanticTokens(document: TextDocument, range: Range, token: CancellationToken): Promise<SemanticTokens> {
    return this.semanticTokensRangeManager.provideDocumentRangeSemanticTokens(document, range, token)
  }

  public async provideInlayHints(document: TextDocument, range: Range, token: CancellationToken): Promise<InlayHintWithProvider[] | null> {
    return this.inlayHintManager.provideInlayHints(document, range, token)
  }

  public async resolveInlayHint(hint: InlayHintWithProvider, token: CancellationToken): Promise<InlayHintWithProvider> {
    return this.inlayHintManager.resolveInlayHint(hint, token)
  }

  public hasLinkedEditing(document: TextDocument): boolean {
    return this.linkedEditingManager.hasProvider(document)
  }

  public async provideLinkedEdits(document: TextDocument, position: Position, token: CancellationToken): Promise<LinkedEditingRanges> {
    return this.linkedEditingManager.provideLinkedEditingRanges(document, position, token)
  }

  public createDiagnosticCollection(owner: string): DiagnosticCollection {
    return diagnosticManager.create(owner)
  }

  public hasProvider(id: string, document: TextDocument): boolean {
    switch (id) {
      case 'formatOnType':
        return this.onTypeFormatManager.hasProvider(document)
      case 'rename':
        return this.renameManager.hasProvider(document)
      case 'onTypeEdit':
        return this.onTypeFormatManager.hasProvider(document)
      case 'documentLink':
        return this.documentLinkManager.hasProvider(document)
      case 'documentColor':
        return this.documentColorManager.hasProvider(document)
      case 'foldingRange':
        return this.foldingRangeManager.hasProvider(document)
      case 'format':
        return this.formatManager.hasProvider(document) || this.formatRangeManager.hasProvider(document)
      case 'codeAction':
        return this.codeActionManager.hasProvider(document)
      case 'workspaceSymbols':
        return this.workspaceSymbolsManager.hasProvider()
      case 'formatRange':
        return this.formatRangeManager.hasProvider(document)
      case 'hover':
        return this.hoverManager.hasProvider(document)
      case 'signature':
        return this.signatureManager.hasProvider(document)
      case 'documentSymbol':
        return this.documentSymbolManager.hasProvider(document)
      case 'documentHighlight':
        return this.documentHighlightManager.hasProvider(document)
      case 'definition':
        return this.definitionManager.hasProvider(document)
      case 'declaration':
        return this.declarationManager.hasProvider(document)
      case 'typeDefinition':
        return this.typeDefinitionManager.hasProvider(document)
      case 'reference':
        return this.referenceManager.hasProvider(document)
      case 'implementation':
        return this.implementationManager.hasProvider(document)
      case 'codeLens':
        return this.codeLensManager.hasProvider(document)
      case 'selectionRange':
        return this.selectionRangeManager.hasProvider(document)
      case 'callHierarchy':
        return this.callHierarchyManager.hasProvider(document)
      case 'semanticTokens':
        return this.semanticTokensManager.hasProvider(document)
      case 'semanticTokensRange':
        return this.semanticTokensRangeManager.hasProvider(document)
      case 'linkedEditing':
        return this.linkedEditingManager.hasProvider(document)
      case 'inlayHint':
        return this.inlayHintManager.hasProvider(document)
      default:
        throw new Error(`Invalid provider name: ${id}`)
    }
  }
}

export default new Languages()
