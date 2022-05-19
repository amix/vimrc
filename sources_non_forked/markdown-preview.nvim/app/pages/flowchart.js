let options = {}

const flowchart = (md, opts = {}) => {
  options = opts
  const temp = md.renderer.rules.fence.bind(md.renderer.rules)
  md.renderer.rules.fence = (tokens, idx, options, env, slf) => {
    const token = tokens[idx]
    try {
      if (token.info && token.info.trim() === 'flowchart') {
        const code = token.content.trim()
        return `<div class="flowchart">${code}</div>`
      }
    } catch (e) {
      console.error(`Parse flowchart Error: `, e)
    }
    return temp(tokens, idx, options, env, slf)
  }
}

export const renderFlowchart = () => {
  let list = document.querySelectorAll('div.flowchart')
  if (!list) {
    return
  }
  list.forEach(item => {
    try {
      let d = window.flowchart.parse(item.textContent);
      item.className = ''
      item.textContent = ''
      d.drawSVG(item, options);
      d = null
    } catch (e) {
      console.error(`Parse flowchart Error: ${e}`)
    }
  })
  list = null
}

export default flowchart
