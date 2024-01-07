local M = {}

function M.configured_lspconfig_servers()
    local configs = require 'lspconfig.configs'
    local keys = {}

    for key, _ in pairs(configs) do
        table.insert(keys, key)
    end

    return keys
end

return M
