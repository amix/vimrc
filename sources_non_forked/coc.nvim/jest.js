const path = require('path')
const os = require('os')
const fs = require('fs')

process.on('uncaughtException', err => {
  let msg = 'Uncaught exception: ' + err.stack
  console.error(msg)
})

process.on('exit', () => {
  fs.rmdirSync(process.env.TMPDIR, { recursive: true, force: true })
})

module.exports = async () => {
  let dataHome = path.join(os.tmpdir(), `coc-test/${process.pid}`)
  fs.mkdirSync(dataHome, { recursive: true })
  process.env.NODE_ENV = 'test'
  process.env.COC_DATA_HOME = dataHome
  process.env.COC_VIMCONFIG = path.join(__dirname, 'src/__tests__')
  process.env.TMPDIR = '/tmp/coc-test'
}
