exports.activate = context => {
  return {
    root: () => {
      return context.extensionPath
    }
  }
}
