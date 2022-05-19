"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const neovim = require('@chemzqm/neovim');
const log4js = require('log4js');
const tslib = require('tslib');
const socketIO = require('socket.io');
const msgpackLite = require('msgpack-lite');
exports.default = {
    '@chemzqm/neovim': neovim,
    log4js,
    tslib,
    'socket.io': socketIO,
    'msgpack-lite': msgpackLite
};
