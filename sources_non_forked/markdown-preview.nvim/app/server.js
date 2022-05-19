exports.run = function () {
  // attach nvim
  const { plugin } = require('./nvim')
  const http = require('http')
  const websocket = require('socket.io')

  const opener = require('./lib/util/opener')
  const logger = require('./lib/util/logger')('app/server')
  const { getIP } = require('./lib/util/getIP')
  const routes = require('./routes')

  let clients = {}

  const openUrl = (url, browser) => {
    const handler = opener(url, browser)
    handler.on('error', (err) => {
      const message = err.message || ''
      const match = message.match(/\s*spawn\s+(.+)\s+ENOENT\s*/)
      if (match) {
        plugin.nvim.call('mkdp#util#echo_messages', ['Error', [`[markdown-preview.nvim]: Can not open browser by using ${match[1]} command`]])
      } else {
        plugin.nvim.call('mkdp#util#echo_messages', ['Error', [err.name, err.message]])
      }
    })
  }

  // http server
  const server = http.createServer(async (req, res) => {
    // plugin
    req.plugin = plugin
    // bufnr
    req.bufnr = (req.headers.referer || req.url)
      .replace(/[?#].*$/, '').split('/').pop()
    // request path
    req.asPath = req.url.replace(/[?#].*$/, '')
    req.mkcss = await plugin.nvim.getVar('mkdp_markdown_css')
    req.hicss = await plugin.nvim.getVar('mkdp_highlight_css')
    // routes
    routes(req, res)
  })

  // websocket server
  const io = websocket(server)

  io.on('connection', async (client) => {
    const { handshake = { query: {} } } = client
    const bufnr = handshake.query.bufnr

    logger.info('client connect: ', client.id, bufnr)

    clients[bufnr] = clients[bufnr] || []
    clients[bufnr].push(client)

    const buffers = await plugin.nvim.buffers
    buffers.forEach(async (buffer) => {
      if (buffer.id === Number(bufnr)) {
        const winline = await plugin.nvim.call('winline')
        const currentWindow = await plugin.nvim.window
        const winheight = await plugin.nvim.call('winheight', currentWindow.id)
        const cursor = await plugin.nvim.call('getpos', '.')
        const options = await plugin.nvim.getVar('mkdp_preview_options')
        const pageTitle = await plugin.nvim.getVar('mkdp_page_title')
        const theme = await plugin.nvim.getVar('mkdp_theme')
        const name = await buffer.name
        const content = await buffer.getLines()
        const currentBuffer = await plugin.nvim.buffer
        client.emit('refresh_content', {
          options,
          isActive: currentBuffer.id === buffer.id,
          winline,
          winheight,
          cursor,
          pageTitle,
          theme,
          name,
          content
        })
      }
    })

    client.on('disconnect', function () {
      logger.info('disconnect: ', client.id)
      clients[bufnr] = (clients[bufnr] || []).map(c => c.id !== client.id)
    })
  })

  async function startServer () {
    const openToTheWord = await plugin.nvim.getVar('mkdp_open_to_the_world')
    const host = openToTheWord ? '0.0.0.0' : '127.0.0.1'
    let port = await plugin.nvim.getVar('mkdp_port')
    port = port || (8080 + Number(`${Date.now()}`.slice(-3)))
    server.listen({
      host,
      port
    }, function () {
      logger.info('server run: ', port)
      function refreshPage ({ bufnr, data }) {
        logger.info('refresh page: ', bufnr)
        ;(clients[bufnr] || []).forEach(c => {
          if (c.connected) {
            c.emit('refresh_content', data)
          }
        })
      }
      function closePage ({ bufnr }) {
        logger.info('close page: ', bufnr)
        clients[bufnr] = (clients[bufnr] || []).filter(c => {
          if (c.connected) {
            c.emit('close_page')
            return false
          }
          return true
        })
      }
      function closeAllPages () {
        logger.info('close all pages')
        Object.keys(clients).forEach(bufnr => {
          ;(clients[bufnr] || []).forEach(c => {
            if (c.connected) {
              c.emit('close_page')
            }
          })
        })
        clients = {}
      }
      async function openBrowser ({ bufnr }) {
        const openIp = await plugin.nvim.getVar('mkdp_open_ip')
        const openHost = openIp !== '' ? openIp : (openToTheWord ? getIP() : 'localhost')
        const url = `http://${openHost}:${port}/page/${bufnr}`
        const browserfunc = await plugin.nvim.getVar('mkdp_browserfunc')
        if (browserfunc !== '') {
          logger.info(`open page [${browserfunc}]: `, url)
          plugin.nvim.call(browserfunc, [url])
        } else {
          const browser = await plugin.nvim.getVar('mkdp_browser')
          logger.info(`open page [${browser || 'default'}]: `, url)
          if (browser !== '') {
            openUrl(url, browser)
          } else {
            openUrl(url)
          }
        }
        const isEchoUrl = await plugin.nvim.getVar('mkdp_echo_preview_url')
        if (isEchoUrl) {
          plugin.nvim.call('mkdp#util#echo_url', [url])
        }
      }
      plugin.init({
        refreshPage,
        closePage,
        closeAllPages,
        openBrowser
      })

      plugin.nvim.call('mkdp#util#open_browser')
    })
  }

  startServer()
}
