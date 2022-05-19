"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const tslib_1 = require("tslib");
const fs_1 = tslib_1.__importDefault(require("fs"));
const module_1 = tslib_1.__importDefault(require("module"));
const path_1 = tslib_1.__importDefault(require("path"));
const vm_1 = tslib_1.__importDefault(require("vm"));
const preloadmodules_1 = tslib_1.__importDefault(require("./preloadmodules"));
function load(scriptPath) {
    const userModule = new module_1.default(scriptPath);
    userModule.filename = scriptPath;
    userModule.paths = module_1.default._nodeModulePaths(path_1.default.dirname(scriptPath));
    const moduleCode = fs_1.default.readFileSync(userModule.filename, 'utf-8');
    userModule.require = userModule.require.bind(userModule);
    const sanbox = vm_1.default.createContext(Object.assign(Object.assign({}, global), { exports: userModule.exports, module: userModule, require: name => {
            if (preloadmodules_1.default[name]) {
                return preloadmodules_1.default[name];
            }
            try {
                return userModule.require(name);
            }
            catch (e) {
                let loadScript = path_1.default.join(path_1.default.dirname(scriptPath), name);
                if (fs_1.default.existsSync(loadScript) && fs_1.default.statSync(loadScript).isDirectory()) {
                    loadScript = path_1.default.join(loadScript, 'index.js');
                }
                else if (!fs_1.default.existsSync(loadScript)) {
                    loadScript = `${loadScript}.js`;
                }
                return load(loadScript);
            }
        }, __filename: userModule.filename, __dirname: path_1.default.dirname(scriptPath), process }));
    vm_1.default.runInContext(moduleCode, sanbox, { filename: userModule.filename });
    return userModule.exports;
}
exports.default = load;
