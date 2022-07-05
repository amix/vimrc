exports.activate = async context => {
  return {
    getContext: () => {
      return context
    }
  }
}
