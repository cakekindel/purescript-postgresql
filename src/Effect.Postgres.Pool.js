import Pg from 'pg'

/** @typedef {{idleTimeout: unknown}} PoolConfigExtra */

/** @type {(o: {unwrapMillis: (_: unknown) => number}) => (cfg: Pg.PoolConfig & PoolConfigExtra & import('./Effect.Postgres.Client.js').ClientConfigExtra) => Pg.PoolConfig} */
export const __uncfg =
  ({ unwrapMillis }) =>
  cfg => {
    if ('idleTimeout' in cfg) {
      cfg.idleTimeoutMillis = unwrapMillis(cfg.idleTimeout)
    }

    return cfg
  }

/** @type {(cfg: Pg.PoolConfig) => () => Pg.Pool} */
export const __make = cfg => () => new Pg.Pool(cfg)

/** @type {(pool: Pg.Pool) => number} */
export const clientCount = pool => pool.totalCount

/** @type {(pool: Pg.Pool) => number} */
export const clientIdleCount = pool => pool.idleCount

/** @type {(pool: Pg.Pool) => number} */
export const clientWaitingCount = pool => pool.waitingCount

/** @type {(pool: Pg.Pool) => (client: Pg.Client | Pg.PoolClient) => (destroy: boolean) => () => void} */
export const __release = _pool => client => destroy => () =>
  'release' in client ? client.release(destroy) : undefined
