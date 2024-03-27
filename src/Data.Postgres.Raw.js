/** @type {(raw: unknown) => string} */
export const rawToString = raw =>
  typeof raw === 'undefined'
    ? 'undefined'
    : typeof raw === 'string'
      ? raw
      : typeof raw === 'number' ||
          typeof raw === 'boolean' ||
          typeof raw === 'symbol'
        ? raw.toString()
        : typeof raw === 'object'
          ? raw === null
            ? 'null'
            : `[${raw.constructor.name}]`
          : 'unknown'

/** @type {(a: unknown) => (b: unknown) => boolean} */
export const rawEq = a => b =>
  typeof a === 'undefined' && typeof b === 'undefined'
    ? true
    : typeof a === typeof b &&
        ['number', 'boolean', 'symbol', 'string'].includes(typeof a)
      ? a === b
      : typeof a === 'object' && typeof b === 'object'
        ? a === null && b === null
          ? true
          : a instanceof Array && b instanceof Array
            ? a.every((a_, ix) => rawEq(a_)(b[ix]))
            : false
        : false
