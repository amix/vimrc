import fs from 'fs'
import log4js from 'log4js'
import os from 'os'
import path from 'path'

const MAX_LOG_SIZE = 1024 * 1024
const MAX_LOG_BACKUPS = 10
const LOG_FILE_PATH =
  process.env.NVIM_MKDP_LOG_FILE || path.join(os.tmpdir(), 'mkdp-nvim.log')

const level = process.env.NVIM_MKDP_LOG_LEVEL || 'info'

if (level === 'debug') {
  fs.writeFileSync(LOG_FILE_PATH, '', 'utf8')
}

const isRoot = process.getuid && process.getuid() === 0

if (!isRoot) {
  log4js.configure({
    appenders: {
      out: {
        type: 'file',
        filename: LOG_FILE_PATH,
        maxLogSize: MAX_LOG_SIZE,
        backups: MAX_LOG_BACKUPS,
        layout: {
          type: 'pattern',
          // Format log in following pattern:
          // yyyy-MM-dd HH:mm:ss.mil $Level (pid:$pid) $categroy - $message.
          pattern: `%d{yyyy-MM-dd hh:mm:ss} %p (pid:${process.pid}) [%c] - %m`
        }
      }
    },
    categories: {
      default: { appenders: ['out'], level }
    }
  })
}

module.exports = (name = 'mkdp'): log4js.Logger => {
  return log4js.getLogger(name)
}
