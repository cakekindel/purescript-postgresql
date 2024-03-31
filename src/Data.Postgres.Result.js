/** @type {(_: import('pg').QueryResult) => Array<unknown>} */
export const rows = r => r.rows

/** @type {(_: import('pg').QueryResult) => number | null} */
export const __rowsAffected = r => r.rowCount
