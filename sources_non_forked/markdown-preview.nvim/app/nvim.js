const attach = require('./lib/attach').default
const logger = require('./lib/util/logger')('app/nvim')

const MSG_PREFIX = '[markdown-preview.nvim]'

const plugin = attach({
  reader: process.stdin,
  writer: process.stdout
})

process.on('uncaughtException', function (err) {
  let msg = `${MSG_PREFIX} uncaught exception: ` + err.stack
  if (plugin.nvim) {
    plugin.nvim.call('mkdp#util#echo_messages', ['Error', msg.split('\n')])
  }
  logger.error('uncaughtException', err.stack)
})

process.on('unhandledRejection', function (reason, p) {
  if (plugin.nvim) {
    plugin.nvim.call('mkdp#util#echo_messages', ['Error', [`${MSG_PREFIX} UnhandledRejection`, `${reason}`]])
  }
  logger.error('unhandledRejection ', p, reason)
})

exports.plugin = plugin
