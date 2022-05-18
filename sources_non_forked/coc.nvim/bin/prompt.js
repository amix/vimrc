/*
 * Used for prompt popup on vim
 */
const readline = require("readline")
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  escapeCodeTimeout: 0
})
rl.setPrompt('')
let value = process.argv[2]
if (value) {
  rl.write(value)
}
rl.on('line', input => {
  let text = input.replace(/"/g, '\\"')
  console.log(createSequences(JSON.stringify(['call', 'CocPopupCallback', ['confirm', text]])))
  process.exit()
})

function createSequences(str) {
  return '\033]51;' + str + '\x07'
}

process.stdin.on('keypress', (_, key) => {
  if (key) {
    let k = getKey(key)
    if (k == '<bs>') {
      return
    }
    if (k == '<cr>') {
      process.exit()
      return
    }
    if (k == '<esc>') {
      console.log(createSequences(JSON.stringify(['call', 'CocPopupCallback', ['exit', '']])))
      process.exit()
      return
    }
    if (k) {
      console.log(createSequences(JSON.stringify(['call', 'CocPopupCallback', ['send', k]])))
    }
  }
})

function getKey(key) {
  if (key.sequence == '\u001b') {
    return '<esc>'
  }
  if (key.sequence == '\r') {
    return '<cr>'
  }
  if (key.sequence == '\t') {
    return key.shift ? '<s-tab>' : '<tab>'
  }
  // handle them can cause bug with terminal
  // if (key.name == 'backspace') {
  //   return '<bs>'
  // }
  // if (key.name == 'left') {
  //   return '<left>'
  // }
  // if (key.name == 'right') {
  //   return '<right>'
  // }
  if (key.name == 'up') {
    return '<up>'
  }
  if (key.name == 'down') {
    return '<down>'
  }
  return ''
}
