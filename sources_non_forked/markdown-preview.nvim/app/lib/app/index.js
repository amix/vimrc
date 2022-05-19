"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const tslib_1 = require("tslib");
const load_1 = tslib_1.__importDefault(require("./load"));
const PATH = '--path';
const VERSION = '--version';
const { argv = [] } = process;
const param = argv[2];
if (param === PATH) {
    (0, load_1.default)(argv[3]).run();
}
else if (param === VERSION) {
    // tslint:disable-next-line
    console.log('0.0.10');
}
