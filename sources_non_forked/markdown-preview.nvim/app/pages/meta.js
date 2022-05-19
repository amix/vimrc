import getRender from 'md-it-meta/lib/meta'

export const meta = separates  => {
  if (separates === void 0) {
    separates = [['---'], ['---']]
  }
  return (md) => {
    const render = getRender(md, separates)
    md.meta = md.meta || {}
    md.block.ruler.before(
      'code',
      'meta',
      (...args) => {
        try {
          return render(...args)
        } catch(e) {
          console.log('md-it-meta', e)
        }
      },
      {
        alt: []
      }
    );
  };
}
