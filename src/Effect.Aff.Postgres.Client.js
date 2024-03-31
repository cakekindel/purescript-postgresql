/** @type {(c: import('pg').Client) => () => Promise<void>} */
export const __connect = c => () => c.connect()

/** @type {(c: import('pg').Client) => () => Promise<void>} */
export const __end = c => () => c.end()

/** @type {(q: import('pg').QueryConfig) => (c: import('pg').Client) => () => Promise<import('pg').QueryResult>} */
export const __query = q => c => () => c.query(q)
