import Chart from 'chart.js'

function render () {
  document.querySelectorAll('.chartjs').forEach(element => {
    try {
      // eslint-disable-next-line no-new
      new Chart(element, JSON.parse(element.textContent))
    } catch (e) {
      element.outerHTML = `<pre>Chart.js complains: "${e}"</pre>`
    }
  })
}

const chartPlugin = (md) => {
  const temp = md.renderer.rules.fence.bind(md.renderer.rules)
  md.renderer.rules.fence = (tokens, idx, options, env, slf) => {
    const token = tokens[idx]
    if (token.info && token.info.trim() === 'chart') {
      const code = token.content.trim()
      try {
        const json = JSON.parse(code)
        return `<canvas class="chartjs">${JSON.stringify(json)}</canvas>`
      } catch (e) { // JSON.parse exception
        return `<pre>${e}</pre>`
      }
    }
    return temp(tokens, idx, options, env, slf)
  }
}

export default {
  render,
  chartPlugin
}
