'use strict'
declare interface Promise<T> {
  /**
   * Catches task error and ignores them.
   */
  logError(): void
}

/**
 * Explicitly tells that promise should be run asynchronously.
 */
Promise.prototype.logError = function <T>(this: Promise<T>): void {
  this.catch(e => {
    require('./logger')('util-extensions').error(e)
  })
}
