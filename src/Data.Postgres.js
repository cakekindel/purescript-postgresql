import { getTypeParser, setTypeParser } from 'pg-types'
import * as Range from 'postgres-range'

export const null_ = null

export const modifyPgTypes = () => {
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
  const asStringArray = getTypeParser(oid['text[]'])
  const asStringRange = Range.parse

  setTypeParser(oid['json'], asString)
  setTypeParser(oid['jsonb'], asString)
  setTypeParser(oid['json[]'], asStringArray)
  setTypeParser(oid['jsonb[]'], asStringArray)

  setTypeParser(oid['timestamp'], asString)
  setTypeParser(oid['timestamptz'], asString)
  setTypeParser(oid['timestamp[]'], asStringArray)
  setTypeParser(oid['timestamptz[]'], asStringArray)
  setTypeParser(oid['tsrange'], asStringRange)
  setTypeParser(oid['tstzrange'], asStringRange)
}
