import * as Pg from 'pg'
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
