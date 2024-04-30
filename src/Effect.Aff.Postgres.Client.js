import QueryStream from 'pg-copy-streams'

/** @type {(c: import('pg').Client) => () => Promise<void>} */
export const __connect = c => () => c.connect()

/** @type {(c: import('pg').Client) => () => Promise<void>} */
export const __end = c => () => c.end()

/** @type {(q: import('pg').QueryConfig) => (c: import('pg').Client) => () => Promise<import('pg').QueryResult>} */
export const __query = q => c => () => c.query(q)

/** @type {(q: string) => (c: import('pg').Client) => () => import('stream').Readable} */
export const __execStreamStdout = q => c => () => c.query(QueryStream.to(q))

/** @type {(q: string) => (c: import('pg').Client) => () => import('stream').Writable} */
export const __execStreamStdin = q => c => () => c.query(QueryStream.from(q))
