let options = {}

const diagram = (md, opts = {}) => {
  options = opts
  const temp = md.renderer.rules.fence.bind(md.renderer.rules)
  md.renderer.rules.fence = (tokens, idx, options, env, slf) => {
    const token = tokens[idx]
    try {
      if (token.info && token.info.trim() === 'sequence-diagrams') {
        const code = token.content.trim()
        return `<div class="sequence-diagrams">${code}</div>`
      }
    } catch (e) {
      console.error(`Parse Diagram Error: `, e)
    }
    return temp(tokens, idx, options, env, slf)
  }
}

export const renderDiagram = () => {
  let list = document.querySelectorAll('.sequence-diagrams')
  if (!list) {
    return
  }
  list.forEach(item => {
    try {
      let d = window.Diagram.parse(item.textContent)
      item.className = ''
      item.textContent = ''
      d.drawSVG(item, {
        theme: 'hand',
        ...options
      })
      d = null
    } catch (e) {
      console.error(`Parse Sequence-diagrams Error: ${e}`)
    }
  })
  list = null
}

export default diagram
