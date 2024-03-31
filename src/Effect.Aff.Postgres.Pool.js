import Pg from 'pg'

/** @type {(pool: Pg.Pool) => () => Promise<void>} */
export const __end = pool => () => pool.end()

/** @type {(pool: Pg.Pool) => () => Promise<Pg.PoolClient>} */
export const __connect = pool => () => pool.connect()
