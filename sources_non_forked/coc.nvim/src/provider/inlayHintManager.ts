'use strict'
import { v4 as uuid } from 'uuid'
import { CancellationToken, Disposable, DocumentSelector, Range } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { InlayHint } from '../inlayHint'
import { comparePosition, positionInRange } from '../util/position'
import { InlayHintsProvider } from './index'
import Manager, { ProviderItem } from './manager'
const logger = require('../util/logger')('inlayHintManger')

export interface InlayHintWithProvider extends InlayHint {
  providerId: string
  resolved?: boolean
}

export default class InlayHintManger extends Manager<InlayHintsProvider> {

  public register(selector: DocumentSelector, provider: InlayHintsProvider): Disposable {
    let item: ProviderItem<InlayHintsProvider> = {
      id: uuid(),
      selector,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  /**
   * Multiple providers can be registered for a language. In that case providers are asked in
   * parallel and the results are merged. A failing provider (rejected promise or exception) will
   * not cause a failure of the whole operation.
   */
  public async provideInlayHints(
    document: TextDocument,
    range: Range,
    token: CancellationToken
  ): Promise<InlayHintWithProvider[] | null> {
    let items = this.getProviders(document)
    if (items.length === 0) return null
    let results: InlayHintWithProvider[] = []
    let finished = 0
    await Promise.all(items.map(item => {
      let { id, provider } = item
      return Promise.resolve(provider.provideInlayHints(document, range, token)).then(hints => {
        if (token.isCancellationRequested) return
        for (let hint of hints) {
          if (!isValidInlayHint(hint, range)) continue
          if (finished > 0 && results.findIndex(o => sameHint(o, hint)) != -1) continue
          results.push({
            providerId: id,
            ...hint
          })
        }
        finished += 1
      }, e => {
        logger.error(`Error on provideInlayHints`, e)
      })
    }))
    return results
  }

  public async resolveInlayHint(hint: InlayHintWithProvider, token: CancellationToken): Promise<InlayHintWithProvider> {
    let provider = this.getProviderById(hint.providerId)
    if (!provider || typeof provider.resolveInlayHint !== 'function' || hint.resolved === true) return hint
    let res = await Promise.resolve(provider.resolveInlayHint(hint, token))
    if (token.isCancellationRequested) return hint
    return Object.assign(hint, res, { resolved: true })
  }
}

export function sameHint(one: InlayHint, other: InlayHint): boolean {
  if (comparePosition(one.position, other.position) !== 0) return false
  return getLabel(one) === getLabel(other)
}

export function isValidInlayHint(hint: InlayHint, range: Range): boolean {
  if (hint.label.length === 0 || (Array.isArray(hint.label) && hint.label.every(part => part.value.length === 0))) {
    logger.warn('INVALID inlay hint, empty label', hint)
    return false
  }
  if (!InlayHint.is(hint)) {
    logger.warn('INVALID inlay hint', hint)
    return false
  }
  if (range && positionInRange(hint.position, range) !== 0) {
    // console.log('INVALID inlay hint, position outside range', range, hint);
    return false
  }
  return true
}

export function getLabel(hint: InlayHint): string {
  if (typeof hint.label === 'string') return hint.label
  return hint.label.map(o => o.value).join(' ')
}
