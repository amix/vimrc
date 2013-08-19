#!/usr/bin/env node

fs = require 'fs'
less = require 'less'
args = process.argv.slice(1)
options = {}

args = args.filter (arg) ->
    match = arg.match(/^-I(.+)$/)
    if match
        options.paths.push(match[1]);
        return false

    match = arg.match(/^--?([a-z][\-0-9a-z]*)(?:=([^\s]+))?$/i)
    if match
        arg = match[1]
    else
        return arg

    switch arg
        when 'strict-imports' then options.strictImports = true
        when 'include-path'
            options.paths = match[2].split(if os.type().match(/Windows/) then ';' else ':')
                .map (p) ->
                    if p
                        return path.resolve(process.cwd(), p)
        when 'O0' then options.optimization = 0
        when 'O1' then options.optimization = 1
        when 'O2' then options.optimization = 2

options.filename = args[1]

parser = new(less.Parser) options

fs.readFile(options.filename, 'utf-8', (err,data) ->
    parser.parse(data, (err, tree) ->
        if err
            less.writeError err
            process.exit(1)
    )
)
