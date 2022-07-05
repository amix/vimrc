import { Neovim } from '@chemzqm/neovim'
import { CancellationToken, CodeAction, Command, CodeActionContext, CodeActionKind, TextEdit, Disposable, Range, Position } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import commands from '../../commands'
import ActionsHandler from '../../handler/codeActions'
import languages from '../../languages'
import { ProviderResult } from '../../provider'
import { disposeAll } from '../../util'
import { rangeInRange } from '../../util/position'
import helper from '../helper'

let nvim: Neovim
let disposables: Disposable[] = []
let codeActions: ActionsHandler
let currActions: CodeAction[]
let resolvedAction: CodeAction
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  codeActions = helper.plugin.getHandler().codeActions
})

afterAll(async () => {
  await helper.shutdown()
})

beforeEach(async () => {
  disposables.push(languages.registerCodeActionProvider([{ language: '*' }], {
    provideCodeActions: (
      _document: TextDocument,
      _range: Range,
      _context: CodeActionContext,
      _token: CancellationToken
    ) => currActions,
    resolveCodeAction: (
      _action: CodeAction,
      _token: CancellationToken
    ): ProviderResult<CodeAction> => resolvedAction
  }, undefined))
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

describe('handler codeActions', () => {
  describe('organizeImport', () => {
    it('should throw error when organize import action not found', async () => {
      currActions = []
      await helper.createDocument()
      let err
      try {
        await codeActions.organizeImport()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })

    it('should perform organize import action', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
      let edits: TextEdit[] = []
      edits.push(TextEdit.replace(Range.create(0, 0, 0, 3), 'bar'))
      edits.push(TextEdit.replace(Range.create(1, 0, 1, 3), 'foo'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('organize import', edit, CodeActionKind.SourceOrganizeImports)
      currActions = [action, CodeAction.create('another action')]
      await codeActions.organizeImport()
      let lines = await doc.buffer.lines
      expect(lines).toEqual(['bar', 'foo'])
    })

    it('should register editor.action.organizeImport command', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
      let edits: TextEdit[] = []
      edits.push(TextEdit.replace(Range.create(0, 0, 0, 3), 'bar'))
      edits.push(TextEdit.replace(Range.create(1, 0, 1, 3), 'foo'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('organize import', edit, CodeActionKind.SourceOrganizeImports)
      currActions = [action, CodeAction.create('another action')]
      await commands.executeCommand('editor.action.organizeImport')
      let lines = await doc.buffer.lines
      expect(lines).toEqual(['bar', 'foo'])
    })
  })

  describe('codeActionRange', () => {
    it('should show warning when no action available', async () => {
      await helper.createDocument()
      currActions = []
      await codeActions.codeActionRange(1, 2, CodeActionKind.QuickFix)
      let line = await helper.getCmdline()
      expect(line).toMatch(/No quickfix code action/)
    })

    it('should apply chosen action', async () => {
      let doc = await helper.createDocument()
      let edits: TextEdit[] = []
      edits.push(TextEdit.insert(Position.create(0, 0), 'bar'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('code fix', edit, CodeActionKind.QuickFix)
      currActions = [action]
      let p = codeActions.codeActionRange(1, 2, CodeActionKind.QuickFix)
      await helper.wait(100)
      await nvim.input('<CR>')
      await p
      let buf = nvim.createBuffer(doc.bufnr)
      let lines = await buf.lines
      expect(lines[0]).toBe('bar')
    })
  })

  describe('getCodeActions', () => {
    it('should get empty actions', async () => {
      currActions = []
      let doc = await helper.createDocument()
      let res = await codeActions.getCodeActions(doc)
      expect(res.length).toBe(0)
    })

    it('should not filter disabled actions', async () => {
      currActions = []
      let action = CodeAction.create('foo', CodeActionKind.QuickFix)
      action.disabled = { reason: 'disabled' }
      currActions.push(action)
      action = CodeAction.create('foo', CodeActionKind.QuickFix)
      action.disabled = { reason: 'disabled' }
      currActions.push(action)
      let doc = await helper.createDocument()
      let res = await codeActions.getCodeActions(doc)
      expect(res.length).toBe(1)
    })

    it('should get all actions', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.setLines(['', '', ''], { start: 0, end: -1, strictIndexing: false })
      let action = CodeAction.create('curr action', CodeActionKind.Empty)
      currActions = [action]
      let range: Range
      disposables.push(languages.registerCodeActionProvider([{ language: '*' }], {
        provideCodeActions: (
          _document: TextDocument,
          r: Range,
          _context: CodeActionContext, _token: CancellationToken
        ) => {
          range = r
          return [CodeAction.create('a'), CodeAction.create('b'), CodeAction.create('c')]
        },
      }, undefined))
      let res = await codeActions.getCodeActions(doc)
      expect(range).toEqual(Range.create(0, 0, 3, 0))
      expect(res.length).toBe(4)
    })

    it('should filter actions by range', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.setLines(['', '', ''], { start: 0, end: -1, strictIndexing: false })
      currActions = []
      let range: Range
      disposables.push(languages.registerCodeActionProvider([{ language: '*' }], {
        provideCodeActions: (
          _document: TextDocument,
          r: Range,
          _context: CodeActionContext, _token: CancellationToken
        ) => {
          range = r
          if (rangeInRange(r, Range.create(0, 0, 1, 0))) return [CodeAction.create('a')]
          return [CodeAction.create('a'), CodeAction.create('b'), CodeAction.create('c')]
        },
      }, undefined))
      let res = await codeActions.getCodeActions(doc, Range.create(0, 0, 0, 0))
      expect(range).toEqual(Range.create(0, 0, 0, 0))
      expect(res.length).toBe(1)
    })

    it('should filter actions by kind prefix', async () => {
      let doc = await helper.createDocument()
      let action = CodeAction.create('my action', CodeActionKind.SourceFixAll)
      currActions = [action]
      let res = await codeActions.getCodeActions(doc, undefined, [CodeActionKind.Source])
      expect(res.length).toBe(1)
      expect(res[0].kind).toBe(CodeActionKind.SourceFixAll)
    })
  })

  describe('getCurrentCodeActions', () => {
    let range: Range
    beforeEach(() => {
      disposables.push(languages.registerCodeActionProvider([{ language: '*' }], {
        provideCodeActions: (
          _document: TextDocument,
          r: Range,
          _context: CodeActionContext, _token: CancellationToken
        ) => {
          range = r
          return [CodeAction.create('a'), CodeAction.create('b'), CodeAction.create('c')]
        },
      }, undefined))
    })

    it('should get codeActions by line', async () => {
      currActions = []
      await helper.createDocument()
      let res = await codeActions.getCurrentCodeActions('line')
      expect(range).toEqual(Range.create(0, 0, 1, 0))
      expect(res.length).toBe(3)
    })

    it('should get codeActions by cursor', async () => {
      currActions = []
      await helper.createDocument()
      let res = await codeActions.getCurrentCodeActions('cursor')
      expect(range).toEqual(Range.create(0, 0, 0, 0))
      expect(res.length).toBe(3)
    })

    it('should get codeActions by visual mode', async () => {
      currActions = []
      await helper.createDocument()
      await nvim.setLine('foo')
      await nvim.command('normal! 0v$')
      await nvim.input('<esc>')
      let res = await codeActions.getCurrentCodeActions('v')
      expect(range).toEqual(Range.create(0, 0, 0, 3))
      expect(res.length).toBe(3)
    })
  })

  describe('doCodeAction', () => {
    it('should not throw when no action exists', async () => {
      currActions = []
      await helper.createDocument()
      let err
      try {
        await codeActions.doCodeAction(undefined)
      } catch (e) {
        err = e
      }
      expect(err).toBeUndefined()
    })

    it('should apply single code action when only is title', async () => {
      let doc = await helper.createDocument()
      let edits: TextEdit[] = []
      edits.push(TextEdit.insert(Position.create(0, 0), 'bar'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('code fix', edit, CodeActionKind.QuickFix)
      currActions = [action]
      await codeActions.doCodeAction(undefined, 'code fix')
      let lines = await doc.buffer.lines
      expect(lines).toEqual(['bar'])
    })

    it('should apply single code action when only is codeAction array', async () => {
      let doc = await helper.createDocument()
      let edits: TextEdit[] = []
      edits.push(TextEdit.insert(Position.create(0, 0), 'bar'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('code fix', edit, CodeActionKind.QuickFix)
      currActions = [action]
      await codeActions.doCodeAction(undefined, [CodeActionKind.QuickFix])
      let lines = await doc.buffer.lines
      expect(lines).toEqual(['bar'])
    })

    it('should show disabled code action', async () => {
      let doc = await helper.createDocument()
      let edits: TextEdit[] = []
      edits.push(TextEdit.insert(Position.create(0, 0), 'bar'))
      let edit = { changes: { [doc.uri]: edits } }
      let refactorAction = CodeAction.create('code refactor', edit, CodeActionKind.Refactor)
      refactorAction.disabled = { reason: 'invalid position' }
      let fixAction = CodeAction.create('code fix', edit, CodeActionKind.QuickFix)
      currActions = [refactorAction, fixAction]
      let p = codeActions.doCodeAction(undefined)
      let winid = await helper.waitFloat()
      let win = nvim.createWindow(winid)
      let buf = await win.buffer
      let lines = await buf.lines
      expect(lines.length).toBe(2)
      expect(lines[1]).toMatch(/code refactor/)
      await nvim.input('2')
      await helper.wait(50)
      await nvim.input('j')
      await nvim.input('<cr>')
      await helper.wait(50)
      let valid = await win.valid
      expect(valid).toBe(true)
      let cmdline = await helper.getCmdline()
      expect(cmdline).toMatch(/invalid position/)
      await nvim.input('<esc>')
    })

    it('should action dialog to choose action', async () => {
      let doc = await helper.createDocument()
      let edits: TextEdit[] = []
      edits.push(TextEdit.insert(Position.create(0, 0), 'bar'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('code fix', edit, CodeActionKind.QuickFix)
      currActions = [action, CodeAction.create('foo')]
      let promise = codeActions.doCodeAction(null)
      await helper.wait(50)
      let ids = await nvim.call('coc#float#get_float_win_list') as number[]
      expect(ids.length).toBeGreaterThan(0)
      await nvim.input('<CR>')
      await promise
      let lines = await doc.buffer.lines
      expect(lines).toEqual(['bar'])
    })

    it('should choose code actions by range', async () => {
      let range: Range
      disposables.push(languages.registerCodeActionProvider([{ language: '*' }], {
        provideCodeActions: (
          _document: TextDocument,
          r: Range,
          _context: CodeActionContext, _token: CancellationToken
        ) => {
          range = r
          return [CodeAction.create('my title'), CodeAction.create('b'), CodeAction.create('c')]
        },
      }, undefined))
      await helper.createDocument()
      await nvim.setLine('abc')
      await nvim.command('normal! 0v$')
      await nvim.input('<esc>')
      await codeActions.doCodeAction('v', 'my title')
      expect(range).toEqual({ start: { line: 0, character: 0 }, end: { line: 0, character: 3 } })
    })
  })

  describe('doQuickfix', () => {
    it('should throw when quickfix action does not exist', async () => {
      let err
      currActions = []
      await helper.createDocument()
      try {
        await codeActions.doQuickfix()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })

    it('should do preferred quickfix action', async () => {
      let doc = await helper.createDocument()
      let edits: TextEdit[] = []
      edits.push(TextEdit.insert(Position.create(0, 0), 'bar'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('code fix', edit, CodeActionKind.QuickFix)
      action.isPreferred = true
      currActions = [CodeAction.create('foo', CodeActionKind.QuickFix), action, CodeAction.create('bar')]
      await codeActions.doQuickfix()
      let lines = await doc.buffer.lines
      expect(lines).toEqual(['bar'])
    })
  })

  describe('applyCodeAction', () => {
    it('should resolve codeAction', async () => {
      let doc = await helper.createDocument()
      let edits: TextEdit[] = []
      edits.push(TextEdit.insert(Position.create(0, 0), 'bar'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('code fix', CodeActionKind.QuickFix)
      action.isPreferred = true
      currActions = [action]
      resolvedAction = Object.assign({ edit }, action)
      let arr = await codeActions.getCurrentCodeActions('line', [CodeActionKind.QuickFix])
      await codeActions.applyCodeAction(arr[0])
      let lines = await doc.buffer.lines
      expect(lines).toEqual(['bar'])
    })

    it('should throw for disabled action', async () => {
      let action: any = CodeAction.create('my action', CodeActionKind.Empty)
      action.disabled = { reason: 'disabled', providerId: 'x' }
      let err
      try {
        await codeActions.applyCodeAction(action)
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })

    it('should invoke registered command after apply edit', async () => {
      let called
      disposables.push(commands.registerCommand('test.execute', async (s: string) => {
        called = s
        await nvim.command(s)
      }))
      let doc = await helper.createDocument()
      let edits: TextEdit[] = []
      edits.push(TextEdit.insert(Position.create(0, 0), 'bar'))
      let edit = { changes: { [doc.uri]: edits } }
      let action = CodeAction.create('code fix', CodeActionKind.QuickFix)
      action.isPreferred = true
      currActions = [action]
      resolvedAction = Object.assign({
        edit,
        command: Command.create('run vim command', 'test.execute', 'normal! $')
      }, action)
      let arr = await codeActions.getCurrentCodeActions('line', [CodeActionKind.QuickFix])
      await codeActions.applyCodeAction(arr[0])
      let lines = await doc.buffer.lines
      expect(lines).toEqual(['bar'])
      expect(called).toBe('normal! $')
    })
  })
})
