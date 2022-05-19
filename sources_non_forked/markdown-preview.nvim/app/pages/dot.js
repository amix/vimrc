let options = {}

const dot = (md, opts = {}) => {
    options = opts
    const temp = md.renderer.rules.fence.bind(md.renderer.rules)
    md.renderer.rules.fence = (tokens, idx, options, env, slf) => {
        const token = tokens[idx]
        try {
            if (token.info && (token.info.trim() === 'dot' || token.info.trim() === 'graphviz')) {
                const code = token.content.trim()
                return `<div class="dot">${code}</div>`
            }
        } catch (e) {
            console.error(`Parse flowchart Error: `, e)
        }
        return temp(tokens, idx, options, env, slf)
    }
}

export const renderDot = () => {
    let list = document.querySelectorAll('.dot')
    if (!list) {
        return
    }
    var viz = new Viz();
    list.forEach(item => {
        viz.renderSVGElement(item.textContent).then(function (e) {
            item.textContent = ''
            item.appendChild(e)
        })
            .catch(e => {
                var viz = new Viz();
                console.error(`Parse dot Error: ${e}`)
            })
    })
    list = null
}

export default dot
