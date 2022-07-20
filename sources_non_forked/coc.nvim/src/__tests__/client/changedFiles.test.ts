/* eslint-disable */
import helper from '../helper'
// import * as assert from 'assert'
import fs from 'fs'
import * as lsclient from '../../language-client'
import * as path from 'path'
import { URI } from 'vscode-uri'
// import which from 'which'

beforeAll(async () => {
  await helper.setup()
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
})

describe('Client integration', () => {

  it('should send file change notification', (done) => {
    if (global.hasOwnProperty('__TEST__')) return done()
    let serverModule = path.join(__dirname, './server/testFileWatcher.js')
    let serverOptions: lsclient.ServerOptions = {
      module: serverModule,
      transport: lsclient.TransportKind.ipc
    }
    let clientOptions: lsclient.LanguageClientOptions = {
      documentSelector: ['css'],
      synchronize: {}, initializationOptions: {},
      middleware: {
      }
    }
    let client = new lsclient.LanguageClient('css', 'Test Language Server', serverOptions, clientOptions)
    let disposable = client.start()

    client.onReady().then(_ => {
      setTimeout(async () => {
        let file = path.join(__dirname, 'test.js')
        fs.writeFileSync(file, '', 'utf8')
        await helper.wait(300)
        let res = await client.sendRequest('custom/received')
        expect(res).toEqual({
          changes: [{
            uri: URI.file(file).toString(),
            type: 1
          }]
        })
        fs.unlinkSync(file)
        disposable.dispose()
        done()
      }, 200)
    }, e => {
      disposable.dispose()
      done(e)
    })
  })

})
