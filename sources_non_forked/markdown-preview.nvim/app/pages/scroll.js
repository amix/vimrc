function scroll (offsetTop) {
  [document.body, document.documentElement].forEach((ele) => {
    // eslint-disable-next-line
    TweenLite.to(
      ele,
      0.4,
      {
        scrollTop: offsetTop,
        ease: Power2.easeOut // eslint-disable-line
      }
    )
  })
}

function getAttrTag (line) {
  return `[data-source-line="${line}"]`
}

function getPreLineOffsetTop (line) {
  let currentLine = line - 1
  let ele = null
  while (currentLine > 0 && !ele) {
    ele = document.querySelector(getAttrTag(currentLine))
    if (!ele) {
      currentLine -= 1
    }
  }
  return [
    currentLine >= 0 ? currentLine : 0,
    ele ? ele.offsetTop : 0
  ]
}

function getNextLineOffsetTop (line, len) {
  let currentLine = line + 1
  let ele = null
  while (currentLine < len && !ele) {
    ele = document.querySelector(getAttrTag(currentLine))
    if (!ele) {
      currentLine += 1
    }
  }
  return [
    currentLine < len ? currentLine : len - 1,
    ele ? ele.offsetTop : document.documentElement.scrollHeight
  ]
}

function topOrBottom (line, len) {
  if (line === 0) {
    scroll(0)
  } else if (line === len - 1) {
    scroll(document.documentElement.scrollHeight)
  }
}

function relativeScroll (line, ratio, len) {
  let offsetTop = 0
  const lineEle = document.querySelector(`[data-source-line="${line}"]`)
  if (lineEle) {
    offsetTop = lineEle.offsetTop
  } else {
    const pre = getPreLineOffsetTop(line)
    const next = getNextLineOffsetTop(line, len)
    offsetTop = pre[1] + ((next[1] - pre[1]) * (line - pre[0]) / (next[0] - pre[0]))
  }
  scroll(offsetTop - document.documentElement.clientHeight * ratio)
}

export default {
  relative: function ({
    cursor,
    winline,
    winheight,
    len
  }) {
    const line = cursor - 1
    const ratio = winline / winheight
    if (line === 0 || line === len - 1) {
      topOrBottom(line, len)
    } else {
      relativeScroll(line, ratio, len)
    }
  },
  middle: function ({
    cursor,
    // winline,
    // winheight,
    len
  }) {
    const line = cursor - 1
    if (line === 0 || line === len - 1) {
      topOrBottom(line, len)
    } else {
      relativeScroll(line, 0.5, len)
    }
  },
  top: function ({
    cursor,
    winline,
    // winheight,
    len
  }) {
    let line = cursor - 1
    if (line === 0 || line === len - 1) {
      topOrBottom(line, len)
    } else {
      line = cursor - winline
      relativeScroll(line, 0, len)
    }
  }
}
