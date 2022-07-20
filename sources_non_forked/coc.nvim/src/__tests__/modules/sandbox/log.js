console.log('log')
console.debug('debug')
console.info('info')
console.error('error')
console.warn('warn')

module.exports = () => {
  return require('coc.nvim')
}
