import PostgresInterval from 'postgres-interval'

/** @typedef {import('postgres-interval').IPostgresInterval} I */

/** @type {(o: {years: number, months: number, days: number, hours: number, minutes: number, seconds: number, milliseconds: number}) => I} */
export const make = o => Object.assign(PostgresInterval(''), o)

/** @type {(s: string) => () => I} */
export const parse = s => () => PostgresInterval(s)

/** @type {(a: I) => number} */
export const getYears = i => i.years || 0.0

/** @type {(a: I) => number} */
export const getMonths = i => i.months || 0.0

/** @type {(a: I) => number} */
export const getDays = i => i.days || 0.0

/** @type {(a: I) => number} */
export const getMinutes = i => i.minutes || 0.0

/** @type {(a: I) => number} */
export const getHours = i => i.hours || 0.0

/** @type {(a: I) => number} */
export const getSeconds = i => i.seconds || 0.0

/** @type {(a: I) => number} */
export const getMilliseconds = i => i.milliseconds || 0.0
