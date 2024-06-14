import Pg from 'pg'
import Range from 'postgres-range'
import { Buffer } from 'buffer'
import PostgresInterval from 'postgres-interval'

/** @type {(a: unknown) => boolean} */
export const isInstanceOfBuffer = a => a instanceof Buffer

/** @type {(a: unknown) => boolean} */
export const isInstanceOfInterval = a => {
  return typeof a === 'object'
    && a !== null
    && ('years' in a
    || 'months' in a
    || 'days' in a
    || 'hours' in a
    || 'minutes' in a
    || 'seconds' in a
    || 'milliseconds' in a
    )
}

export const modifyPgTypes = () => {
  // https://github.com/brianc/node-pg-types/blob/master/lib/textParsers.js
  const oid = {
    'text[]': 1009,
    json: 114,
    jsonb: 3802,
    'json[]': 199,
    'jsonb[]': 3807,
    timestamp: 1114,
    timestamptz: 1184,
    'timestamp[]': 1115,
    'timestamptz[]': 1185,
    tsrange: 3908,
    tstzrange: 3910,
  }

  // @ts-ignore
  const asString = a => a
  const asStringArray = Pg.types.getTypeParser(oid['text[]'])
  const asStringRange = Range.parse

  Pg.types.setTypeParser(oid['json'], asString)
  Pg.types.setTypeParser(oid['jsonb'], asString)
  Pg.types.setTypeParser(oid['json[]'], asStringArray)
  Pg.types.setTypeParser(oid['jsonb[]'], asStringArray)

  Pg.types.setTypeParser(oid['timestamp'], asString)
  Pg.types.setTypeParser(oid['timestamptz'], asString)
  Pg.types.setTypeParser(oid['timestamp[]'], asStringArray)
  Pg.types.setTypeParser(oid['timestamptz[]'], asStringArray)
  Pg.types.setTypeParser(oid['tsrange'], asStringRange)
  Pg.types.setTypeParser(oid['tstzrange'], asStringRange)
}
