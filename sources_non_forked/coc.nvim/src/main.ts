'use strict'
Object.defineProperty(console, 'log', {
  value() {
    if (logger) logger.info(...arguments)
  }
})
import './util/extensions'
import attach from './attach'
const logger = require('./util/logger')('server')

attach({ reader: process.stdin, writer: process.stdout })

process.on('uncaughtException', function(err) {
  let msg = 'Uncaught exception: ' + err.message
  console.error(msg)
  logger.error('uncaughtException', err.stack)
})

process.on('unhandledRejection', function(reason, p) {
  if (reason instanceof Error) {
    console.error('UnhandledRejection: ' + reason.message + '\n' + reason.stack)
  } else {
    console.error('UnhandledRejection: ' + reason)
  }
  logger.error('unhandledRejection ', p, reason)
})
