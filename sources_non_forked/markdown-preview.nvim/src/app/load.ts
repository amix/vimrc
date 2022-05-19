import fs from 'fs'
import Module from 'module'
import path from 'path'
import vm from 'vm'

import modules from './preloadmodules'

export default function load(scriptPath) {
  const userModule = new Module(scriptPath)
  userModule.filename = scriptPath
  userModule.paths = (Module as any)._nodeModulePaths(path.dirname(scriptPath))

  const moduleCode = fs.readFileSync(userModule.filename, 'utf-8')

  userModule.require = userModule.require.bind(userModule)

  const sanbox = vm.createContext({
    ...global,
    exports: userModule.exports,
    module: userModule,
    require: name => {
      if (modules[name]) {
        return modules[name]
      }
      try {
        return userModule.require(name)
      } catch (e) {
        let loadScript = path.join(path.dirname(scriptPath), name)
        if (fs.existsSync(loadScript) && fs.statSync(loadScript).isDirectory()) {
          loadScript = path.join(loadScript, 'index.js')
        } else if (!fs.existsSync(loadScript)) {
          loadScript = `${loadScript}.js`
        }
        return load(loadScript)
      }
    },
    __filename: userModule.filename,
    __dirname: path.dirname(scriptPath),
    process,
  })

  vm.runInContext(moduleCode, sanbox, { filename: userModule.filename })

  return userModule.exports
}
