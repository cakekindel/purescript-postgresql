import {readFile, writeFile} from 'fs/promises'
import {execSync} from 'child_process'

let ver = process.argv[2]
if (!ver) {
  console.error(`tag required: bun bun/prepare.js v1.0.0`)
} else if (!/v\d+\.\d+\.\d+/.test(ver)) {
  console.error(`invalid tag: ${ver}`)
}

ver = (/\d+\.\d+\.\d+/.exec(ver) || [])[0] || ''

const pkg = await readFile('./package.json', 'utf8')
const pkgnew = pkg.replace(/"version": ".+"/, `"version": "v${ver}"`)
await writeFile('./package.json', pkgnew)

const spago = await readFile('./spago.yaml', 'utf8')
const spagonew = spago.replace(/version: .+/, `version: '${ver}'`)
await writeFile('./spago.yaml', spagonew)

const readme = await readFile('./README.md', 'utf8')
const readmenew = readme.replace(/packages\/purescript-postgresql\/.+?\//g, `/packages/purescript-postgresql/${ver}/`)
await writeFile('./README.md', readmenew)

execSync(`git add spago.yaml package.json README.md`)
execSync(`git commit -m 'chore: prepare v${ver}'`)
execSync(`git tag v${ver}`)
execSync(`git push --tags`)
execSync(`git push --mirror github-mirror`)
