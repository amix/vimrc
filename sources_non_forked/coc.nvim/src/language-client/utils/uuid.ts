'use strict'
import { v4 as uuidv4 } from 'uuid'

export function generateUuid(): string {
  return uuidv4()
}
