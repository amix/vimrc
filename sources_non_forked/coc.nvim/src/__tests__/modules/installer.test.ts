import { registryUrl, getInstallArguments, getDependencies, Installer } from '../../model/installer'
import { rmdir } from '../helper'
import workspace from '../../workspace'
import path from 'path'
import which from 'which'
import fs from 'fs-extra'
import os from 'os'

const rcfile = path.join(os.tmpdir(), '.npmrc')

describe('Installer', () => {
  it('should get registry url', async () => {
    fs.writeFileSync(rcfile, '', 'utf8')
    expect(registryUrl()).toBe('https://registry.npmjs.org/')
    fs.writeFileSync(rcfile, 'coc.nvim:registry=https://example.org', 'utf8')
    expect(registryUrl()).toBe('https://example.org/')
    fs.writeFileSync(rcfile, 'registry=https://example.org/', 'utf8')
    expect(registryUrl()).toBe('https://example.org/')
    if (fs.existsSync(rcfile)) {
      fs.unlinkSync(rcfile)
    }
  })

  it('should get install arguments', async () => {
    expect(getInstallArguments('pnpm', 'https://github.com/')).toEqual(['install'])
    expect(getInstallArguments('npm', '')).toEqual(['install', '--ignore-scripts', '--no-lockfile', '--production', '--legacy-peer-deps', '--no-global'])
    expect(getInstallArguments('yarn', '')).toEqual(['install', '--ignore-scripts', '--no-lockfile', '--production', '--ignore-engines'])
  })

  it('should get dependencies', async () => {
    expect(getDependencies('{}')).toEqual({})
    expect(getDependencies('')).toEqual({})
    expect(getDependencies('{"dependencies":{"foo": "0.0.1"}}')).toEqual({ foo: '0.0.1' })
    expect(getDependencies('{"dependencies":{"coc.nvim": "0.0.80"}}')).toEqual({})
  })

  it('should parse name & version', async () => {
    const getInfo = (def: string): { name?: string, version?: string } => {
      let installer = new Installer(__dirname, 'npm', def)
      return installer.info
    }
    expect(getInfo('https://github.com')).toEqual({ name: undefined, version: undefined })
    expect(getInfo('@yaegassy/coc-intelephense')).toEqual({ name: '@yaegassy/coc-intelephense', version: undefined })
    expect(getInfo('@yaegassy/coc-intelephense@1.0.0')).toEqual({ name: '@yaegassy/coc-intelephense', version: '1.0.0' })
    expect(getInfo('foo@1.0.0')).toEqual({ name: 'foo', version: '1.0.0' })
  })

  it('should throw for url that not supported', async () => {
    let installer = new Installer(__dirname, 'npm', 'https://example.com')
    let fn = async () => {
      await installer.getInfoFromUri()
    }
    await expect(fn()).rejects.toThrow(/not supported/)
  })

  it('should get info from url', async () => {
    let installer = new Installer(__dirname, 'npm', 'https://github.com/sdras/vue-vscode-snippets@main')
    let info = await installer.getInfoFromUri()
    expect(info['dist.tarball']).toMatch(/main.tar.gz/)
  }, 10000)

  it('should skip install & update for symbolic folder', async () => {
    let tmpDir = path.join(os.tmpdir(), 'foo')
    if (fs.existsSync(tmpDir)) {
      fs.unlinkSync(tmpDir)
    }
    fs.symlinkSync(__dirname, tmpDir, 'dir')
    let installer = new Installer(os.tmpdir(), 'npm', 'foo')
    let res = await installer.doInstall({ name: 'foo' })
    expect(res).toBe(false)
    let val = await installer.update()
    expect(val).toBeUndefined()
    fs.unlinkSync(tmpDir)
  })

  it('should skip update when current version is latest', async () => {
    let dir = path.join(os.tmpdir(), 'coc-pairs')
    let installer = new Installer(os.tmpdir(), 'npm', 'coc-pairs')
    let info = await installer.getInfo()
    fs.mkdirSync(dir)
    fs.writeFileSync(path.join(dir, 'package.json'), `{"version": "${info.version}"}`, 'utf8')
    let res = await installer.update()
    expect(res).toBeUndefined()
    rmdir(dir)
  }, 20000)

  it('should skip update when version not satisfies', async () => {
    let v = workspace.version
    Object.assign(workspace, { version: '0.0.10' })
    let installer = new Installer(os.tmpdir(), 'npm', 'coc-pairs')
    let dir = path.join(os.tmpdir(), 'coc-pairs')
    fs.mkdirSync(dir, { recursive: true })
    let fn = async () => {
      await installer.update()
    }
    await expect(fn()).rejects.toThrow(/please update/)
    Object.assign(workspace, { version: v })
    rmdir(dir)
  })

  it('should update extension', async () => {
    let installer = new Installer(os.tmpdir(), 'npm', 'coc-pairs')
    let dir = path.join(os.tmpdir(), 'coc-pairs')
    fs.mkdirSync(dir, { recursive: true })
    let res = await installer.update()
    expect(res).toMatch(/coc-pairs/)
    rmdir(dir)
  }, 30000)

  it('should install extension with dependencies', async () => {
    let npm: string
    try {
      npm = which.sync('pnpm')
    } catch (e) {
      npm = which.sync('npm')
    }
    // coc-html use typescript as dependencies
    let installer = new Installer(os.tmpdir(), npm, 'coc-html')
    await installer.install()
    let folder = path.join(os.tmpdir(), 'coc-html')
    expect(fs.existsSync(path.join(folder, 'node_modules/typescript/package.json'))).toBe(true)
    fs.unlinkSync(path.join(os.tmpdir(), 'package.json'))
    rmdir(folder)
  }, 30000)
})
