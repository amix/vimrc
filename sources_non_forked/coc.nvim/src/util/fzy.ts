'use strict'
let SCORE_MIN = -Infinity
let SCORE_MAX = Infinity

let SCORE_GAP_LEADING = -0.005
let SCORE_GAP_TRAILING = -0.005
let SCORE_GAP_INNER = -0.01
let SCORE_MATCH_CONSECUTIVE = 1
let SCORE_MATCH_SLASH = 0.9
let SCORE_MATCH_WORD = 0.8
let SCORE_MATCH_CAPITAL = 0.7
let SCORE_MATCH_DOT = 0.6

function islower(s): boolean {
  return s.toLowerCase() === s
}

function isupper(s): boolean {
  return s.toUpperCase() === s
}

function precompute_bonus(haystack): number[] {
  /* Which positions are beginning of words */
  let m = haystack.length
  let match_bonus = new Array(m)

  let last_ch = '/'
  for (let i = 0; i < m; i++) {
    let ch = haystack[i]

    if (last_ch === '/') {
      match_bonus[i] = SCORE_MATCH_SLASH
    } else if (last_ch === '-' || last_ch === '_' || last_ch === ' ') {
      match_bonus[i] = SCORE_MATCH_WORD
    } else if (last_ch === '.') {
      match_bonus[i] = SCORE_MATCH_DOT
    } else if (islower(last_ch) && isupper(ch)) {
      match_bonus[i] = SCORE_MATCH_CAPITAL
    } else {
      match_bonus[i] = 0
    }

    last_ch = ch
  }

  return match_bonus
}

function compute(needle, haystack, D, M): void {
  let n = needle.length
  let m = haystack.length

  let lower_needle = needle.toLowerCase()
  let lower_haystack = haystack.toLowerCase()

  let match_bonus = precompute_bonus(haystack)

  /*
   * D[][] Stores the best score for this position ending with a match.
   * M[][] Stores the best possible score at this position.
   */

  for (let i = 0; i < n; i++) {
    D[i] = new Array(m)
    M[i] = new Array(m)

    let prev_score = SCORE_MIN
    let gap_score = i === n - 1 ? SCORE_GAP_TRAILING : SCORE_GAP_INNER

    for (let j = 0; j < m; j++) {
      if (lower_needle[i] === lower_haystack[j]) {
        let score = SCORE_MIN
        if (!i) {
          score = (j * SCORE_GAP_LEADING) + match_bonus[j]
        } else if (j) { /* i > 0 && j > 0*/
          score = Math.max(
            M[i - 1][j - 1] + match_bonus[j],

            /* consecutive match, doesn't stack with match_bonus */
            D[i - 1][j - 1] + SCORE_MATCH_CONSECUTIVE)
        }
        D[i][j] = score
        M[i][j] = prev_score = Math.max(score, prev_score + gap_score)
      } else {
        D[i][j] = SCORE_MIN
        M[i][j] = prev_score = prev_score + gap_score
      }
    }
  }
}

export function score(needle, haystack): number {
  let n = needle.length
  let m = haystack.length

  if (!n || !m)
    return SCORE_MIN

  if (n === m) {
    /* Since this method can only be called with a haystack which
     * matches needle. If the lengths of the strings are equal the
     * strings themselves must also be equal (ignoring case).
     */
    return SCORE_MAX
  }

  if (m > 1024) {
    /*
     * Unreasonably large candidate: return no score
     * If it is a valid match it will still be returned, it will
     * just be ranked below any reasonably sized candidates
     */
    return SCORE_MIN
  }

  let D = new Array(n)
  let M = new Array(n)

  compute(needle, haystack, D, M)

  return M[n - 1][m - 1]
}

export function groupPositions(arr: number[]): [number, number][] {
  let res: [number, number][] = []
  for (let i = 0; i < arr.length; i++) {
    let last = res.length ? res[res.length - 1] : undefined
    let curr = arr[i]
    if (last && curr == last[1]) {
      last[1] = curr + 1
    } else {
      res.push([curr, curr + 1])
    }
  }
  return res
}

export function positions(needle: string, haystack: string): number[] {
  let n = needle.length
  let m = haystack.length

  let positions = new Array(n)

  if (!n || !m)
    return positions

  if (n === m) {
    for (let i = 0; i < n; i++)
      positions[i] = i
    return positions
  }

  if (m > 1024) {
    return positions
  }

  let D = new Array(n)
  let M = new Array(n)

  compute(needle, haystack, D, M)

  /* backtrack to find the positions of optimal matching */
  let match_required = false

  for (let i = n - 1, j = m - 1; i >= 0; i--) {
    for (; j >= 0; j--) {
      /*
       * There may be multiple paths which result in
       * the optimal weight.
       *
       * For simplicity, we will pick the first one
       * we encounter, the latest in the candidate
       * string.
       */
      if (D[i][j] !== SCORE_MIN &&
        (match_required || D[i][j] === M[i][j])) {
        /* If this score was determined using
         * SCORE_MATCH_CONSECUTIVE, the
         * previous character MUST be a match
         */
        match_required =
          i && j &&
          M[i][j] === D[i - 1][j - 1] + SCORE_MATCH_CONSECUTIVE
        positions[i] = j--
        break
      }
    }
  }

  return positions
}

export function hasMatch(needle: string, haystack: string): boolean {
  needle = needle.toLowerCase()
  haystack = haystack.toLowerCase()
  let l = needle.length
  for (let i = 0, j = 0; i < l; i += 1) {
    j = haystack.indexOf(needle[i], j) + 1
    if (j === 0) return false
  }
  return true
}
