function resolveHtmlImage (tokens, idx) {
  let content = tokens[idx].content || ''

  content = content.replace(/<img\s+([^>]*?)src\s*=\s*(["'])([^\2>]+?)\2([^>]*)>/gm, (m, g1, g2, g3, g4) => {
    if (/^(http|\/\/|data:)/.test(g3)) {
      return m
    }
    return `<img ${g1}src="/_local_image_${encodeURIComponent(g3)}"${g4}>`
  })

  return content
}

function resolveImage (tokens, idx) {
  const src = tokens[idx].attrs[0][1]
  const alt = tokens[idx].content
  const resAttrs = tokens[idx].attrs.slice(2).reduce((pre, cur) => `${pre} ${cur[0]}=${cur[1]}`, '')
  if (/^(http|\/\/|data:)/.test(src)) {
    return `<img src="${src}" alt="${alt}" ${resAttrs} />`
  }
  return `<img src="/_local_image_${encodeURIComponent(src)}" alt="${alt}" ${resAttrs} />`
}

export default function localImage (md) {
  md.renderer.rules.image = resolveImage
  md.renderer.rules.html_block = resolveHtmlImage
  md.renderer.rules.html_inline = resolveHtmlImage
}
