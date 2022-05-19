/*
 * Used for prompt popup on vim
 */
const readline = require("readline")
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  escapeCodeTimeout: 0,
  prompt: ''
})
rl.setPrompt('')
let value = process.argv[2]
if (value) {
  rl.write(value)
}
rl.on('line', input => {
  send(['confirm', input])
  process.exit()
})

let original_ttyWrite = rl._ttyWrite
rl._ttyWrite = function (code, key) {
  if (key.name === 'enter') {
    send(['send', '<C-j>'])
    return ''
  }
  original_ttyWrite.apply(rl, arguments)
  send(['change', rl.line])
}

function createSequences(str) {
  return '\033]51;' + str + '\x07'
}

function send(args) {
  process.stdout.write(createSequences(JSON.stringify(['call', 'CocPopupCallback', args])))
}

process.stdin.on('keypress', (e, key) => {
  if (key) {
    let k = getKey(key)
    if (k == '<bs>') {
      return
    }
    if (k == '<esc>') {
      send(['exit', ''])
      process.exit()
      return
    }
    if (k) {
      send(['send', k])
      return
    }
  }
})

function getKey(key) {
  if (key.ctrl === true) {
    if (key.name == 'n') {
      return '<C-n>'
    }
    if (key.name == 'p') {
      return '<C-p>'
    }
    if (key.name == 'j') {
      return '<C-j>'
    }
    if (key.name == 'k') {
      return '<C-k>'
    }
    if (key.name == 'f') {
      return '<C-f>'
    }
    if (key.name == 'b') {
      return '<C-b>'
    }
    if (key.sequence == '\x00') {
      return '<C-@>'
    }
  }
  if (key.sequence == '\u001b') {
    return '<esc>'
  }
  if (key.sequence == '\r') {
    return '<cr>'
  }
  if (key.sequence == '\t') {
    return key.shift ? '<s-tab>' : '<tab>'
  }
  if (key.name == 'up') {
    return '<up>'
  }
  if (key.name == 'down') {
    return '<down>'
  }
  return ''
}
