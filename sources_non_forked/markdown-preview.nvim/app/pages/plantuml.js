const plantumlEncoder = require("plantuml-encoder");

function generateSourceDefault (umlCode, pluginOptions) {
  var imageFormat = pluginOptions.imageFormat || 'img'
  var diagramName = pluginOptions.diagramName || 'uml'
  var server = pluginOptions.server || 'https://www.plantuml.com/plantuml'
  var zippedCode = plantumlEncoder.encode(umlCode)

  return server + '/' + imageFormat + '/' + zippedCode
}

export default (md, opts = {}) => {
  const temp = md.renderer.rules.fence.bind(md.renderer.rules)
  md.renderer.rules.fence = (tokens, idx, options, env, slf) => {
    const token = tokens[idx]
    try {
      if (token.info && token.info.indexOf('plantuml') != -1 ) {
        const code = token.content.trim()
        return `<img src="${generateSourceDefault(code, opts)}" alt="" />`
      }
    } catch (e) {
      console.error(`Parse Diagram Error: `, e)
    }
    return temp(tokens, idx, options, env, slf)
  }
}
