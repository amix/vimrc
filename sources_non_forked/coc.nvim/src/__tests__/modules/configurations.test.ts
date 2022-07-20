import fs from 'fs-extra'
import * as assert from 'assert'
import { ParseError } from 'jsonc-parser'
import os from 'os'
import path from 'path'
import { v1 as uuidv1 } from 'uuid'
import { URI } from 'vscode-uri'
import Configurations from '../../configuration'
import ConfigurationProxy from '../../configuration/shape'
import { convertErrors, removeFromValueTree, getChangedKeys, getConfigurationValue, getKeys, mergeConfigProperties, parseConfiguration, parseContentFromFile } from '../../configuration/util'
import { ConfigurationTarget, IConfigurationModel } from '../../types'
import { CONFIG_FILE_NAME, wait } from '../../util'
import { rmdir } from '../helper'

const config = fs.readFileSync(path.join(__dirname, './settings.json'), 'utf8')
const workspaceConfigFile = path.resolve(__dirname, `../sample/.vim/${CONFIG_FILE_NAME}`)

function getConfigurationModel(): IConfigurationModel {
  let [, contents] = parseConfiguration(config)
  return { contents }
}

function U(fsPath: string): string {
  return URI.file(fsPath).toString()
}

function createConfigurations(): Configurations {
  let userConfigFile = path.join(__dirname, './settings.json')
  return new Configurations(userConfigFile)
}

afterEach(() => {
  global.__TEST__ = true
})

describe('ConfigurationProxy', () => {
  it('should not throw when URI is not valid', async () => {
    let proxy = new ConfigurationProxy({})
    proxy.modifyConfiguration(undefined, 'foo')
    proxy.modifyConfiguration(URI.parse('ftp:///f'), 'foo')
  })

  it('should create file and parent folder when necessary', async () => {
    let folder = path.join(os.tmpdir(), 'a')
    if (fs.existsSync(folder)) {
      let isFile = fs.statSync(folder).isFile()
      if (isFile) {
        fs.unlinkSync(folder)
      } else {
        rmdir(folder)
      }
    }
    let uri = URI.file(path.join(os.tmpdir(), 'a/b/settings.json'))
    let proxy = new ConfigurationProxy({})
    proxy.modifyConfiguration(uri, 'foo', true)
    let content = fs.readFileSync(uri.fsPath, 'utf8')
    expect(JSON.parse(content)).toEqual({ foo: true })
    rmdir(folder)
  })

  it('should get folder from resolver', async () => {
    let proxy = new ConfigurationProxy({
      getWorkspaceFolder: (uri: string) => {
        let fsPath = URI.parse(uri).fsPath
        if (fsPath.startsWith(os.tmpdir())) {
          return { uri: URI.file(os.tmpdir()).toString(), name: 'tmp' }
        }
        if (fsPath.startsWith(os.homedir())) {
          return { uri: URI.file(os.homedir()).toString(), name: 'home' }
        }
        return undefined
      },
      root: __dirname
    })
    let uri = proxy.getWorkspaceConfig(URI.file(__filename).toString())
    expect(uri).toBeUndefined()
    uri = proxy.getWorkspaceConfig(URI.file(path.join(os.tmpdir(), 'foo')).toString())
    expect(uri.fsPath.startsWith(os.tmpdir())).toBe(true)
    uri = proxy.getWorkspaceConfig()
    expect(uri.fsPath.startsWith(__dirname)).toBe(true)
    uri = proxy.getWorkspaceConfig(URI.file(path.join(os.homedir(), 'tmp')).toString())
    expect(uri).toBeUndefined()
    proxy = new ConfigurationProxy({})
    uri = proxy.getWorkspaceConfig(URI.file(path.join(os.tmpdir(), 'foo')).toString())
    expect(uri).toBeUndefined()
    uri = proxy.getWorkspaceConfig()
    expect(uri).toBeUndefined()
  })

  it('should update and remove configuration option', async () => {
    let fsPath = path.join(os.tmpdir(), 'my-settings.json')
    fs.writeFileSync(fsPath, '{"foo": false}')
    let proxy = new ConfigurationProxy({})
    proxy.$updateConfigurationOption(ConfigurationTarget.Workspace, 'bar', true, { resource: URI.file(fsPath) })

    let content = fs.readFileSync(fsPath, 'utf8')
    expect(JSON.parse(content)).toEqual({ foo: false, bar: true })
    proxy.$removeConfigurationOption(ConfigurationTarget.Workspace, 'bar', { resource: URI.file(fsPath) })
    content = fs.readFileSync(fsPath, 'utf8')
    expect(JSON.parse(content)).toEqual({ foo: false })
    fs.unlinkSync(fsPath)
  })
})

describe('parse configuration', () => {
  it('should only split top level dot keys', () => {
    let o = { 'x.y': 'foo' }
    let [, contents] = parseConfiguration(JSON.stringify(o))
    expect(contents).toEqual({ x: { y: 'foo' } })
    let schema = { 'my.schema': { 'foo.bar': 1 } }
    let [, obj] = parseConfiguration(JSON.stringify(schema))
    expect(obj).toEqual({ my: { schema: { 'foo.bar': 1 } } })
  })

  it('should not parse uri properties', async () => {
    let o: any = {
      foo: {
        'bar://x': '',
        'file://y': ''
      }
    }
    let [, contents] = parseConfiguration(JSON.stringify(o))
    expect(contents).toEqual({
      foo: {
        'bar://x': '',
        'file://y': ''
      }
    })
  })

  it('should merge preperties', async () => {
    let res = mergeConfigProperties({
      foo: 'bar',
      "x.y.a": "x",
      "x.y.b": "y",
      "x.t": "z"
    })
    expect(res).toEqual({
      foo: 'bar', x: { y: { a: 'x', b: 'y' }, t: 'z' }
    })
  })
})

describe('Configurations', () => {
  describe('utils', () => {
    it('removeFromValueTree: remove a non existing key', () => {
      let target = { a: { b: 2 } }

      removeFromValueTree(target, 'c')

      assert.deepStrictEqual(target, { a: { b: 2 } })
    })

    it('removeFromValueTree: remove a multi segmented key from an object that has only sub sections of the key', () => {
      let target = { a: { b: 2 } }

      removeFromValueTree(target, 'a.b.c')

      assert.deepStrictEqual(target, { a: { b: 2 } })
    })

    it('removeFromValueTree: remove a single segmented key', () => {
      let target = { a: 1 }

      removeFromValueTree(target, 'a')

      assert.deepStrictEqual(target, {})
    })

    it('removeFromValueTree: remove a single segmented key when its value is undefined', () => {
      let target = { a: undefined }

      removeFromValueTree(target, 'a')

      assert.deepStrictEqual(target, {})
    })

    it('removeFromValueTree: remove a multi segmented key when its value is undefined', () => {
      let target = { a: { b: 1 } }

      removeFromValueTree(target, 'a.b')

      assert.deepStrictEqual(target, {})
    })

    it('removeFromValueTree: remove a multi segmented key when its value is array', () => {
      let target = { a: { b: [1] } }

      removeFromValueTree(target, 'a.b')

      assert.deepStrictEqual(target, {})
    })

    it('removeFromValueTree: remove a multi segmented key first segment value is array', () => {
      let target = { a: [1] }

      removeFromValueTree(target, 'a.0')

      assert.deepStrictEqual(target, { a: [1] })
    })

    it('removeFromValueTree: remove when key is the first segment', () => {
      let target = { a: { b: 1 } }

      removeFromValueTree(target, 'a')

      assert.deepStrictEqual(target, {})
    })

    it('removeFromValueTree: remove a multi segmented key when the first node has more values', () => {
      let target = { a: { b: { c: 1 }, d: 1 } }

      removeFromValueTree(target, 'a.b.c')

      assert.deepStrictEqual(target, { a: { d: 1 } })
    })

    it('removeFromValueTree: remove a multi segmented key when in between node has more values', () => {
      let target = { a: { b: { c: { d: 1 }, d: 1 } } }

      removeFromValueTree(target, 'a.b.c.d')

      assert.deepStrictEqual(target, { a: { b: { d: 1 } } })
    })

    it('removeFromValueTree: remove a multi segmented key when the last but one node has more values', () => {
      let target = { a: { b: { c: 1, d: 1 } } }

      removeFromValueTree(target, 'a.b.c')

      assert.deepStrictEqual(target, { a: { b: { d: 1 } } })
    })

    it('should parse content from file', async () => {
      let res = parseContentFromFile('')
      expect(res).toEqual({ contents: {} })
    })

    it('should convert errors', () => {
      let errors: ParseError[] = []
      for (let i = 0; i < 17; i++) {
        errors.push({
          error: i,
          offset: 0,
          length: 10
        })
      }
      let res = convertErrors('file:///1', 'abc', errors)
      expect(res.length).toBe(17)
    })

    it('should get all keys', () => {
      let res = getKeys({
        foo: {
          bar: 1,
          from: {
            to: 2
          }
        },
        bar: [1, 2]
      })
      expect(res).toEqual(['foo', 'foo.bar', 'foo.from', 'foo.from.to', 'bar'])
    })

    it('should get configuration value', () => {
      let root = {
        foo: {
          bar: 1,
          from: {
            to: 2
          }
        },
        bar: [1, 2]
      }
      let res = getConfigurationValue(root, 'foo.from.to', 1)
      expect(res).toBe(2)
      res = getConfigurationValue(root, 'foo.from', 1)
      expect(res).toEqual({ to: 2 })
    })

    it('should get changed keys #1', () => {
      let res = getChangedKeys({ y: 2 }, { x: 1 })
      expect(res).toEqual(['x', 'y'])
    })

    it('should get changed keys #2', () => {
      let res = getChangedKeys({ x: 1, c: { d: 4 } }, { x: 1, b: { x: 5 } })
      expect(res).toEqual(['b', 'b.x', 'c', 'c.d'])
    })

    it('should parse configurations', () => {
      let { contents } = getConfigurationModel()
      expect(contents.foo.bar).toBe(1)
      expect(contents.bar.foo).toBe(2)
      expect(contents.schema).toEqual({ 'https://example.com': '*.yaml' })
    })
  })

  describe('addFolderFile()', () => {
    it('should add folder as workspace configuration', () => {
      let configurations = createConfigurations()
      configurations.onDidChange(e => {
        let affects = e.affectsConfiguration('coc')
        expect(affects).toBe(true)
      })
      configurations.addFolderFile(workspaceConfigFile)
      let o = configurations.configuration.workspace.contents
      expect(o.coc.preferences.rootPath).toBe('./src')
      configurations.dispose()
    })

    it('should not add invalid folders', async () => {
      let configurations = createConfigurations()
      expect(configurations.addFolderFile('ab')).toBe(false)
      let configFile = path.join(__dirname, 'settings.json')
      expect(configurations.addFolderFile(configFile)).toBe(false)
      configFile = path.join(os.homedir(), '.vim/coc-settings.json')
      expect(configurations.addFolderFile(configFile)).toBe(false)
    })

    it('should resolve folder configuration when possible', async () => {
      let configurations = createConfigurations()
      expect(configurations.resolveFolderConfigution('test:///foo')).toBeUndefined()
      expect(configurations.resolveFolderConfigution(URI.file(path.join(os.tmpdir(), 'foo')).toString())).toBeUndefined()
      let fsPath = path.join(__dirname, `../sample/abc`)
      expect(configurations.resolveFolderConfigution(URI.file(fsPath).toString())).toBeDefined()
      fsPath = path.join(__dirname, `../sample/foo`)
      expect(configurations.resolveFolderConfigution(URI.file(fsPath).toString())).toBeDefined()
    })
  })

  describe('getConfiguration()', () => {
    it('should load default configurations', () => {
      let conf = new Configurations()
      expect(conf.defaults.contents.coc).toBeDefined()
      let c = conf.getConfiguration('languageserver')
      expect(c).toEqual({})
      expect(c.has('not_exists')).toBe(false)
      conf.dispose()
    })

    it('should inspect configuration', async () => {
      let conf = new Configurations()
      let c = conf.getConfiguration('suggest')
      let res = c.inspect('not_exists')
      expect(res.defaultValue).toBeUndefined()
      expect(res.globalValue).toBeUndefined()
      expect(res.workspaceValue).toBeUndefined()
    })

    it('should update user config #1', () => {
      let conf = new Configurations()
      let fn = jest.fn()
      conf.onDidChange(e => {
        expect(e.affectsConfiguration('x')).toBe(true)
        fn()
      })
      conf.updateUserConfig({ x: 1 })
      let config = conf.configuration.user
      expect(config.contents).toEqual({ x: 1 })
      expect(fn).toBeCalled()
    })

    it('should update user config #2', () => {
      let conf = new Configurations()
      conf.updateUserConfig({ x: 1 })
      conf.updateUserConfig({ x: undefined })
      let config = conf.configuration.user
      expect(config.contents).toEqual({})
    })

    it('should update workspace config #1', () => {
      let conf = new Configurations()
      conf.updateUserConfig({ foo: { bar: 1 } })
      let curr = conf.getConfiguration('foo')
      curr.update('bar', 2, false)
      curr = conf.getConfiguration('foo')
      let n = curr.get<number>('bar')
      expect(n).toBe(2)
    })

    it('should update workspace config by create workspace folder settings', async () => {
      let folder = path.join(os.tmpdir(), 'a')
      let proxy = new ConfigurationProxy({
        getWorkspaceFolder: (uri: string) => {
          let fsPath = URI.parse(uri).fsPath
          if (fsPath.startsWith(folder)) {
            return { uri: U(folder), name: 'tmp' }
          }
          return undefined
        },
        root: __dirname
      })
      let conf = new Configurations('', proxy)
      let fn = jest.fn()
      let resource = U(path.join(folder, 'foo'))
      conf.onDidChange(e => {
        if (e.affectsConfiguration('foo', resource.toString())) {
          fn()
        }
      })
      global.__TEST__ = false
      let curr = conf.getConfiguration(undefined, resource)
      curr.update('foo', true)
      expect(fn).toBeCalled()
      let filepath = path.join(folder, '.vim/coc-settings.json')
      let content = fs.readFileSync(filepath, 'utf8')
      expect(JSON.parse(content)).toEqual({ foo: true })
      let res = conf.getConfiguration(undefined, resource)
      expect(res.foo).toBe(true)
      curr = conf.getConfiguration(undefined, resource)
      curr.update('foo', undefined)
      content = fs.readFileSync(filepath, 'utf8')
      expect(JSON.parse(content)).toEqual({})
      rmdir(folder)
    })

    it('should handle errors', () => {
      let tmpFile = path.join(os.tmpdir(), uuidv1())
      fs.writeFileSync(tmpFile, '{"x":', 'utf8')
      let conf = new Configurations(tmpFile)
      let errors = conf.errorItems
      expect(errors.length > 1).toBe(true)
      conf.dispose()
    })

    it('should change to new folder configuration', () => {
      let conf = new Configurations()
      conf.addFolderFile(workspaceConfigFile)
      let configFile = path.join(__dirname, './settings.json')
      conf.addFolderFile(configFile)
      let file = path.resolve(__dirname, '../sample/tmp.js')
      let fn = jest.fn()
      conf.onDidChange(fn)
      conf.setFolderConfiguration(URI.file(file).toString())
      let { contents } = conf.workspace
      expect(contents.foo).toBeUndefined()
      expect(fn).toBeCalled()
      conf.dispose()
    })

    it('should get nested property', () => {
      let config = createConfigurations()
      let conf = config.getConfiguration('servers.c')
      let res = conf.get<string>('trace.server', '')
      expect(res).toBe('verbose')
      config.dispose()
    })

    it('should get user and workspace configuration', () => {
      let userConfigFile = path.join(__dirname, './settings.json')
      let configurations = new Configurations(userConfigFile)
      let data = configurations.configuration.toData()
      expect(data.user).toBeDefined()
      expect(data.workspace).toBeDefined()
      expect(data.defaults).toBeDefined()
      let value = configurations.configuration.getValue()
      expect(value.foo).toBeDefined()
      expect(value.foo.bar).toBe(1)
      configurations.dispose()
    })

    it('should override with new value', () => {
      let configurations = createConfigurations()
      configurations.configuration.defaults.setValue('foo', 1)
      let { contents } = configurations.defaults
      expect(contents.foo).toBe(1)
      configurations.dispose()
    })

    it('should extends defaults', () => {
      let configurations = createConfigurations()
      configurations.extendsDefaults({ 'a.b': 1 })
      configurations.extendsDefaults({ 'a.b': 2 })
      let o = configurations.defaults.contents
      expect(o.a.b).toBe(2)
      configurations.dispose()
    })

    it('should update configuration', async () => {
      let configurations = createConfigurations()
      configurations.addFolderFile(workspaceConfigFile)
      let fn = jest.fn()
      configurations.onDidChange(e => {
        expect(e.affectsConfiguration('foo')).toBe(true)
        expect(e.affectsConfiguration('foo.bar')).toBe(true)
        expect(e.affectsConfiguration('foo.bar', 'file://tmp/foo.js')).toBe(false)
        fn()
      })
      let config = configurations.getConfiguration('foo')
      let o = config.get<number>('bar')
      expect(o).toBe(1)
      config.update('bar', 6)
      config = configurations.getConfiguration('foo')
      expect(config.get<number>('bar')).toBe(6)
      expect(fn).toBeCalledTimes(1)
      configurations.dispose()
    })

    it('should remove configuration', async () => {
      let configurations = createConfigurations()
      configurations.addFolderFile(workspaceConfigFile)
      let fn = jest.fn()
      configurations.onDidChange(e => {
        expect(e.affectsConfiguration('foo')).toBe(true)
        expect(e.affectsConfiguration('foo.bar')).toBe(true)
        fn()
      })
      let config = configurations.getConfiguration('foo')
      let o = config.get<number>('bar')
      expect(o).toBe(1)
      config.update('bar', null, true)
      config = configurations.getConfiguration('foo')
      expect(config.get<any>('bar')).toBeUndefined()
      expect(fn).toBeCalledTimes(1)
      configurations.dispose()
    })
  })

  describe('watchFile', () => {
    it('should watch user config file', async () => {
      global.__TEST__ = false
      let userConfigFile = path.join(os.tmpdir(), 'settings.json')
      fs.writeFileSync(userConfigFile, '{"foo.bar": true}', { encoding: 'utf8' })
      let conf = new Configurations(userConfigFile)
      await wait(20)
      fs.writeFileSync(userConfigFile, '{"foo.bar": false}', { encoding: 'utf8' })
      await wait(150)
      let c = conf.getConfiguration('foo')
      let res = c.get('bar')
      expect(res).toBe(false)
      conf.dispose()
      fs.unlinkSync(userConfigFile)
    })

    it('should watch workspace config file', async () => {
      global.__TEST__ = false
      let configFile = path.join(os.tmpdir(), '.vim/coc-settings.json')
      fs.mkdirSync(path.join(os.tmpdir(), '.vim'), { recursive: true })
      fs.writeFileSync(configFile, '{"foo.bar": true}', { encoding: 'utf8' })
      let conf = new Configurations('', {
        $updateConfigurationOption: () => {},
        $removeConfigurationOption: () => {},
        getWorkspaceConfig: () => {
          return URI.file(configFile)
        }
      })
      let uri = U(path.join(os.tmpdir(), 'foo'))
      let resolved = conf.resolveFolderConfigution(uri)
      conf.setFolderConfiguration(uri)
      expect(resolved).toBeDefined()
      await wait(20)
      fs.writeFileSync(configFile, '{"foo.bar": false}', { encoding: 'utf8' })
      await wait(150)
      let c = conf.getConfiguration('foo')
      let res = c.get('bar')
      expect(res).toBe(false)
      conf.dispose()
      if (fs.existsSync(configFile)) fs.unlinkSync(configFile)
    })
  })

  describe('getFolderConfiguration()', () => {
    it('should get folder configuration from uri', async () => {
      let conf = new Configurations()
      conf.cwd = os.tmpdir()
      let res = conf.getFolderConfiguration('untitled:///1')
      expect(res[0]).toBeUndefined()
      conf = createConfigurations()
      res = conf.getFolderConfiguration('untitled:///1')
      expect(res[0]).toBeDefined()
    })

    it('should get folder configuration from file', async () => {
      let conf = createConfigurations()
      let fsPath = path.join(os.tmpdir(), 'a')
      let res = conf.getFolderConfiguration(U(fsPath))
      expect(res[0]).toBeUndefined()
    })

    it('should not throw when workspace config not resolved', async () => {
      let userConfigFile = path.join(__dirname, './settings.json')
      let conf = new Configurations(userConfigFile, {
        $updateConfigurationOption: () => {},
        $removeConfigurationOption: () => {},
        getWorkspaceConfig: () => {
          return URI.file(userConfigFile)
        }
      })
      let fsPath = path.join(os.tmpdir(), 'a')
      let c = conf.getConfiguration(undefined, U(fsPath))
      c.update('foo', false)
    })
  })

  describe('getWorkspaceConfigUri()', () => {
    it('should get config uri for undefined resource', async () => {
      let conf = createConfigurations()
      let res = conf.getWorkspaceConfigUri()
      expect(res).toBeDefined()
    })

    it('should not get config uri same as user config', async () => {
      let userConfigFile = path.join(__dirname, './settings.json')
      let conf = new Configurations(userConfigFile, {
        $updateConfigurationOption: () => {},
        $removeConfigurationOption: () => {},
        getWorkspaceConfig: () => {
          return URI.file(userConfigFile)
        }
      })
      let uri = U(__filename)
      let res = conf.getWorkspaceConfigUri(uri)
      expect(res).toBeUndefined()
    })
  })
})
