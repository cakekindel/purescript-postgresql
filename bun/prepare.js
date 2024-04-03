import {readFile, writeFile} from 'fs/promises'
import {execSync} from 'child_process'

const ver = process.argv[2]
if (!ver) {
  console.error(`tag required: bun bun/prepare.js v1.0.0`)
} else if (!/v\d+\.\d+\.\d+/.test(ver)) {
  console.error(`invalid tag: ${ver}`)
}

const pkg = await readFile('./package.json', 'utf8')
const pkgnew = pkg.replace(/"version": ".+"/, `"version": "${ver}"`)
await writeFile('./package.json', pkgnew)

const spago = await readFile('./spago.yaml', 'utf8')
const spagonew = spago.replace(/version: .+/, `version: '${ver}'`)
await writeFile('./spago.yaml', spagonew)

const readme = await readFile('./README.md', 'utf8')
const readmenew = readme.replace(/packages\/postgresql\/.+?\//g, `/packages/postgresql/${ver}/`)
await writeFile('./README.md', readmenew)

execSync(`git tag ${ver}`)
execSync(`git push --tags`)
execSync(`git push --mirror github-mirror`)
