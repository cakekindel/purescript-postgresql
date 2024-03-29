/** @type {(_: Buffer) => () => bigint} */
export const readBigInt64BE = buf => () => buf.readBigInt64BE(0)

export const dbg = a => () => typeof a === 'string' ? console.log(Buffer.from(a).toString('hex')) : undefined
