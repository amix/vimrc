import { Neovim } from '@chemzqm/neovim'
import { Disposable, LocationLink, Location, Range, Position, CancellationTokenSource } from 'vscode-languageserver-protocol'
import LocationHandler from '../../handler/locations'
import languages from '../../languages'
import services from '../../services'
import workspace from '../../workspace'
import { disposeAll } from '../../util'
import helper from '../helper'
import { URI } from 'vscode-uri'

let nvim: Neovim
let locations: LocationHandler
let disposables: Disposable[] = []
let currLocations: Location[]
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  Object.assign(workspace.env, {
    locationlist: false
  })
  locations = helper.plugin.getHandler().locations
})

afterAll(async () => {
  await helper.shutdown()
})

beforeEach(async () => {
  await helper.createDocument()
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

function createLocation(name: string, sl: number, sc: number, el: number, ec: number): Location {
  return Location.create(`test://${name}`, Range.create(sl, sc, el, ec))
}

describe('locations', () => {
  describe('no provider', () => {
    it('should return null when provider does not exist', async () => {
      let doc = (await workspace.document).textDocument
      let pos = Position.create(0, 0)
      let tokenSource = new CancellationTokenSource()
      let token = tokenSource.token
      expect(await languages.getDefinition(doc, pos, token)).toBe(null)
      expect(await languages.getDefinitionLinks(doc, pos, token)).toBe(null)
      expect(await languages.getDeclaration(doc, pos, token)).toBe(null)
      expect(await languages.getTypeDefinition(doc, pos, token)).toBe(null)
      expect(await languages.getImplementation(doc, pos, token)).toBe(null)
      expect(await languages.getReferences(doc, { includeDeclaration: false }, pos, token)).toBe(null)
    })
  })

  describe('reference', () => {
    beforeEach(() => {
      disposables.push(languages.registerReferencesProvider([{ language: '*' }], {
        provideReferences: () => {
          return currLocations
        }
      }))
    })

    it('should get references', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0), createLocation('bar', 0, 0, 0, 0)]
      let res = await locations.references()
      expect(res.length).toBe(2)
    })

    it('should jump to references', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0)]
      let res = await locations.gotoReferences('edit', true)
      expect(res).toBe(true)
      let name = await nvim.call('bufname', ['%'])
      expect(name).toBe('test://foo')
    })

    it('should return false when references not found', async () => {
      currLocations = []
      let res = await locations.gotoReferences('edit', true)
      expect(res).toBe(false)
    })
  })

  describe('definition', () => {
    beforeEach(() => {
      disposables.push(languages.registerDefinitionProvider([{ language: '*' }], {
        provideDefinition: () => {
          return currLocations
        }
      }))
    })

    it('should get definitions', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0), createLocation('bar', 0, 0, 0, 0)]
      let res = await locations.definitions()
      expect(res.length).toBe(2)
    })

    it('should jump to definitions', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0)]
      let res = await locations.gotoDefinition('edit')
      expect(res).toBe(true)
      let name = await nvim.call('bufname', ['%'])
      expect(name).toBe('test://foo')
    })

    it('should return false when definitions not found', async () => {
      currLocations = []
      let res = await locations.gotoDefinition('edit')
      expect(res).toBe(false)
    })
  })

  describe('declaration', () => {
    beforeEach(() => {
      disposables.push(languages.registerDeclarationProvider([{ language: '*' }], {
        provideDeclaration: () => {
          return currLocations
        }
      }))
    })

    it('should get declarations', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0), createLocation('bar', 0, 0, 0, 0)]
      let res = await locations.declarations() as Location[]
      expect(res.length).toBe(2)
    })

    it('should jump to declaration', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0)]
      let res = await locations.gotoDeclaration('edit')
      expect(res).toBe(true)
      let name = await nvim.call('bufname', ['%'])
      expect(name).toBe('test://foo')
    })

    it('should return false when declaration not found', async () => {
      currLocations = []
      let res = await locations.gotoDeclaration('edit')
      expect(res).toBe(false)
    })
  })

  describe('typeDefinition', () => {
    beforeEach(() => {
      disposables.push(languages.registerTypeDefinitionProvider([{ language: '*' }], {
        provideTypeDefinition: () => {
          return currLocations
        }
      }))
    })

    it('should get type definition', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0), createLocation('bar', 0, 0, 0, 0)]
      let res = await locations.typeDefinitions() as Location[]
      expect(res.length).toBe(2)
    })

    it('should jump to type definition', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0)]
      let res = await locations.gotoTypeDefinition('edit')
      expect(res).toBe(true)
      let name = await nvim.call('bufname', ['%'])
      expect(name).toBe('test://foo')
    })

    it('should return false when type definition not found', async () => {
      currLocations = []
      let res = await locations.gotoTypeDefinition('edit')
      expect(res).toBe(false)
    })
  })

  describe('implementation', () => {
    beforeEach(() => {
      disposables.push(languages.registerImplementationProvider([{ language: '*' }], {
        provideImplementation: () => {
          return currLocations
        }
      }))
    })

    it('should get implementations', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0), createLocation('bar', 0, 0, 0, 0)]
      let res = await locations.implementations() as Location[]
      expect(res.length).toBe(2)
    })

    it('should jump to implementation', async () => {
      currLocations = [createLocation('foo', 0, 0, 0, 0)]
      let res = await locations.gotoImplementation('edit')
      expect(res).toBe(true)
      let name = await nvim.call('bufname', ['%'])
      expect(name).toBe('test://foo')
    })

    it('should return false when implementation not found', async () => {
      currLocations = []
      let res = await locations.gotoImplementation('edit')
      expect(res).toBe(false)
    })
  })

  describe('getTagList', () => {
    it('should return null when cword does not exist', async () => {
      let res = await locations.getTagList()
      expect(res).toBe(null)
    })

    it('should return null when provider does not exist', async () => {
      await nvim.setLine('foo')
      await nvim.command('normal! ^')
      let res = await locations.getTagList()
      expect(res).toBe(null)
    })

    it('should return null when result is empty', async () => {
      disposables.push(languages.registerDefinitionProvider([{ language: '*' }], {
        provideDefinition: () => {
          return []
        }
      }))
      await nvim.setLine('foo')
      await nvim.command('normal! ^')
      let res = await locations.getTagList()
      expect(res).toBe(null)
    })

    it('should return tag definitions', async () => {
      disposables.push(languages.registerDefinitionProvider([{ language: '*' }], {
        provideDefinition: () => {
          return [createLocation('bar', 2, 0, 2, 5), Location.create(URI.file('/foo').toString(), Range.create(1, 0, 1, 5))]
        }
      }))
      await nvim.setLine('foo')
      await nvim.command('normal! ^')
      let res = await locations.getTagList()
      expect(res).toEqual([
        {
          name: 'foo',
          cmd: 'keepjumps 3 | normal 1|',
          filename: 'test://bar'
        },
        { name: 'foo', cmd: 'keepjumps 2 | normal 1|', filename: '/foo' }
      ])
    })
  })

  describe('findLocations', () => {
    // hook result
    let fn
    let result: any
    beforeAll(() => {
      fn = services.sendRequest
      services.sendRequest = () => {
        return Promise.resolve(result)
      }
    })

    afterAll(() => {
      services.sendRequest = fn
    })

    it('should handle locations from language client', async () => {
      result = [createLocation('bar', 2, 0, 2, 5)]
      await locations.findLocations('foo', 'mylocation', {}, false)
      let res = await nvim.getVar('coc_jump_locations')
      expect(res).toEqual([{
        uri: 'test://bar',
        lnum: 3,
        end_lnum: 3,
        col: 1,
        end_col: 6,
        filename: 'test://bar',
        text: '',
        range: Range.create(2, 0, 2, 5)
      }])
    })

    it('should handle nested locations', async () => {
      let location: any = {
        location: createLocation('file', 0, 0, 0, 0),
        children: [{
          location: createLocation('foo', 3, 0, 3, 5),
          children: []
        }, {
          location: createLocation('bar', 4, 0, 4, 5),
          children: []
        }]
      }
      result = location
      await locations.findLocations('foo', 'mylocation', {}, false)
      let res = await nvim.getVar('coc_jump_locations') as any[]
      expect(res.length).toBe(3)
    })
  })

  describe('handleLocations', () => {
    it('should not throw when location is undefined', async () => {
      await locations.handleLocations(null)
    })

    it('should not throw when locations is empty array', async () => {
      await locations.handleLocations([])
    })

    it('should handle single location', async () => {
      await locations.handleLocations(createLocation('single', 0, 0, 0, 0))
      let bufname = await nvim.call('bufname', ['%'])
      expect(bufname).toBe('test://single')
    })

    it('should handle location link', async () => {
      let link = LocationLink.create('test://link', Range.create(0, 0, 0, 3), Range.create(1, 0, 1, 3))
      await locations.handleLocations([link])
      let bufname = await nvim.call('bufname', ['%'])
      expect(bufname).toBe('test://link')
    })
  })
})
