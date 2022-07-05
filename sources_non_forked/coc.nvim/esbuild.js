const cp = require('child_process')
const fs = require('fs')
const path = require('path')
let revision = 'master'
if (process.env.NODE_ENV !== 'development') {
  try {
    let res = cp.execSync(`git log -1 --date=iso --pretty=format:'"%h","%ad"'`, {encoding: 'utf8'})
    revision = res.replaceAll('"', '').replace(',', ' ')
  } catch (e) {
    // ignore
  }
}

let envPlugin = {
  name: 'env',
  setup(build) {
    build.onResolve({filter: /\/appenders$/}, args => {
      let fullpath = path.join(args.resolveDir, args.path)
      return {
        path: path.relative(__dirname, fullpath).replace(/\\/g, '/'),
        namespace: 'env-ns'
      }
    })
    build.onLoad({filter: /^node_modules\/log4js\/lib\/appenders$/, namespace: 'env-ns'}, args => {
      let content = fs.readFileSync(path.join(args.path, 'index.js'), 'utf8')
      return {
        contents: content.replace(/require\.main/g, '""'),
        resolveDir: args.path
      }
    })
  }
}

async function start(watch) {
  await require('esbuild').build({
    entryPoints: ['src/main.ts'],
    bundle: true,
    watch,
    minify: process.env.NODE_ENV === 'production',
    sourcemap: process.env.NODE_ENV === 'development',
    define: {REVISION: '"' + revision + '"', ESBUILD: 'true'},
    mainFields: ['module', 'main'],
    platform: 'node',
    target: 'node12.12',
    outfile: 'build/index.js',
    banner: {
      js: `(function () {
  var v = process.version
  var parts = v.slice(1).split('.')
  var major = parseInt(parts[0], 10)
  var minor = parseInt(parts[1], 10)
  if (major < 12 || (major == 12 && minor < 12)) {
    throw new Error('coc.nvim requires node >= v12.12.0, current version: ' + v)
  }
})(); `
    },
    plugins: [envPlugin]
  })
}

let watch = false
if (process.argv.includes('--watch')) {
  console.log('watching...')
  watch = {
    onRebuild(error) {
      if (error) {
        console.error('watch build failed:', error)
      } else {
        console.log('watch build succeeded')
      }
    },
  }
}

start(watch).catch(e => {
  console.error(e)
})
