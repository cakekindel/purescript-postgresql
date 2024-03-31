import Pg from 'pg'

/** @typedef {{statementTimeout: unknown, queryTimeout: unknown, idleInTransactionTimeout: unknown, connectionTimeout: unknown, applicationName: string}} ClientConfigExtra */

/** @type {(_o: {unwrapMillis: (ms: unknown) => number}) => (cfg: Pg.ClientConfig & ClientConfigExtra) => Pg.ClientConfig} */
export const __uncfg =
  ({ unwrapMillis }) =>
  cfg => {
    if ('statementTimeout' in cfg) {
      cfg.statement_timeout = unwrapMillis(cfg.statementTimeout)
    }
    if ('queryTimeout' in cfg) {
      cfg.query_timeout = unwrapMillis(cfg.queryTimeout)
    }
    if ('idleInTransactionTimeout' in cfg) {
      cfg.idle_in_transaction_session_timeout = unwrapMillis(
        cfg.idleInTransactionTimeout,
      )
    }
    if ('connectionTimeout' in cfg) {
      cfg.connectionTimeoutMillis = unwrapMillis(cfg.connectionTimeout)
    }
    if ('applicationName' in cfg) {
      cfg.application_name = cfg.applicationName
    }
    return cfg
  }

/** @type {(cfg: Pg.ClientConfig) => () => Pg.Client} */
export const __make = cfg => () => new Pg.Client(cfg)
