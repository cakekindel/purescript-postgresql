import * as Range from 'postgres-range'

/**
 * @template T
 * @typedef {{upper: T | undefined, lower: T | undefined, lowerIncl: boolean, upperIncl: boolean}} RangeRawRecord
 */

/** @type {<T>(_: Range.Range<T>) => RangeRawRecord<T>} */
export const __rangeRawToRecord = r => {
  if (r.hasMask(Range.RANGE_EMPTY)) {
    return {
      upper: undefined,
      lower: undefined,
      lowerIncl: false,
      upperIncl: false,
    }
  } else {
    const upper = r.upper === null ? undefined : r.upper
    const lower = r.lower === null ? undefined : r.lower
    return {
      upper: r.hasMask(Range.RANGE_UB_INF) ? undefined : upper,
      lower: r.hasMask(Range.RANGE_LB_INF) ? undefined : lower,
      lowerIncl: r.hasMask(Range.RANGE_LB_INC),
      upperIncl: r.hasMask(Range.RANGE_UB_INC),
    }
  }
}

/** @type {<T>(_: RangeRawRecord<T>) => Range.Range<T>} */
export const __rangeRawFromRecord = r => {
  const upper = r.upper === undefined ? null : r.upper
  const lower = r.lower === undefined ? null : r.lower
  if (upper === null && lower === null) {
    // @ts-ignore
    return new Range.Range(lower, upper, Range.RANGE_EMPTY)
  }

  let mask = 0
  if (upper === null) {
    mask |= Range.RANGE_UB_INF
  } else if (r.upperIncl) {
    mask |= Range.RANGE_UB_INC
  }

  if (lower === null) {
    mask |= Range.RANGE_LB_INF
  } else if (r.lowerIncl) {
    mask |= Range.RANGE_LB_INC
  }

  return new Range.Range(lower, upper, mask)
}

/** @type {<T>(r: Range.Range<T>) => () => string} */
export const __rangeRawSerialize = r => () => {
  return Range.serialize(r)
}

/** @type {<T>(r: string) => (f: (s: string) => () => T) => () => Range.Range<T>} */
export const __rangeRawParse = r => f => () => {
  return Range.parse(r, s => f(s)())
}

/** @type {(r: unknown) => () => Range.Range<unknown>} */
export const __rangeRawFromRaw = r => () => {
  if (r instanceof Range.Range) {
    return r
  } else {
    throw new TypeError(`expected instance of Range, found ${r}`)
  }
}
