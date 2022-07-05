exports.activate = async context => {
  return {
    asAbsolutePath: p => {
      return context.asAbsolutePath(p)
    },
    getContext: () => {
      return context
    },
    echo: x => {
      return x
    }
  }
}
