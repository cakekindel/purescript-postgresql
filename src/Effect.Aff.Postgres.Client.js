/** @type {(c: import('pg').Client) => () => Promise<void>} */
export const connectImpl = c => () => c.connect()

/** @type {(c: import('pg').Client) => () => Promise<void>} */
export const endImpl = c => () => c.end()

/** @type {(q: import('pg').QueryConfig) => (c: import('pg').Client) => () => Promise<import('pg').QueryResult>} */
export const queryImpl = q => c => () => c.query(q)
