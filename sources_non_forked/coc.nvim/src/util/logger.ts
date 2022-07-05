'use strict'
import fs from 'fs'
import log4js from 'log4js'
import path from 'path'
import os from 'os'
import { mkdirpSync } from 'fs-extra'

function getLogFile(): string {
  let file = process.env.NVIM_COC_LOG_FILE
  if (file) return file
  let dir = process.env.XDG_RUNTIME_DIR
  if (dir) {
    try {
      fs.accessSync(dir, fs.constants.R_OK | fs.constants.W_OK)
      return path.join(dir, `coc-nvim-${process.pid}.log`)
    } catch (err) {
      // noop
    }
  }
  let tmpdir = os.tmpdir()
  dir = path.join(tmpdir, `coc.nvim-${process.pid}`)
  if (!fs.existsSync(dir)) mkdirpSync(dir)
  return path.join(dir, `coc-nvim.log`)
}

const MAX_LOG_SIZE = 1024 * 1024
const MAX_LOG_BACKUPS = 10
let logfile = getLogFile()
const level = process.env.NVIM_COC_LOG_LEVEL || 'info'

if (fs.existsSync(logfile)) {
  // cleanup if exists
  try {
    fs.writeFileSync(logfile, '', { encoding: 'utf8', mode: 0o666 })
  } catch (e) {
    // noop
  }
}

exports.getLogFile = getLogFile

log4js.configure({
  disableClustering: true,
  appenders: {
    out: {
      type: 'file',
      mode: 0o666,
      filename: logfile,
      maxLogSize: MAX_LOG_SIZE,
      backups: MAX_LOG_BACKUPS,
      layout: {
        type: 'pattern',
        // Format log in following pattern:
        // yyyy-MM-dd HH:mm:ss.mil $Level (pid:$pid) $categroy - $message.
        pattern: `%d{ISO8601} %p (pid:${process.pid}) [%c] - %m`,
      },
    }
  },
  categories: {
    default: { appenders: ['out'], level }
  }
})

module.exports = (name = 'coc-nvim'): log4js.Logger => {
  let logger = log4js.getLogger(name)
  return Object.assign(logger, {
    getLogFile,
    logfile
  })
}
