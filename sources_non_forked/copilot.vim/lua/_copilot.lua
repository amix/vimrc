local copilot = {}

copilot.lsp_start_client = function(cmd, handler_names)
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  local handlers = {}
  local id
  for _, name in ipairs(handler_names) do
    handlers[name] = function(err, result)
      if result then
        local retval = vim.call('copilot#agent#LspHandle', id, {method = name, params = result})
        if type(retval) == 'table' then return retval.result, retval.error end
      end
    end
    if name:match('^copilot/') then
      capabilities.copilot = capabilities.copilot or {}
      capabilities.copilot[name:match('^copilot/(.*)$')] = true
    end
  end
  id = vim.lsp.start_client({
    cmd = cmd,
    cmd_cwd = vim.call('copilot#job#Cwd'),
    name = 'copilot',
    capabilities = capabilities,
    handlers = handlers,
    get_language_id = function(bufnr, filetype)
      return vim.call('copilot#doc#LanguageForFileType', filetype)
    end,
    on_init = function(client, initialize_result)
      vim.call('copilot#agent#LspInit', client.id, initialize_result)
    end,
    on_exit = function(code, signal, client_id)
      vim.schedule(function()
        vim.call('copilot#agent#LspExit', client_id, code, signal)
      end)
    end
  })
  return id
end

copilot.lsp_request = function(client_id, method, params)
  local client = vim.lsp.get_client_by_id(client_id)
  if not client then return end
  vim.lsp.buf_attach_client(0, client_id)
  for _, doc in ipairs({params.doc, params.textDocument}) do
    if doc and type(doc.uri) == 'number' then
      local bufnr = doc.uri
      vim.lsp.buf_attach_client(bufnr, client_id)
      doc.uri = vim.uri_from_bufnr(bufnr)
      doc.version = vim.lsp.util.buf_versions[bufnr]
    end
  end
  local _, id
  _, id = client.request(method, params, function(err, result)
    vim.call('copilot#agent#LspResponse', client_id, {id = id, error = err, result = result})
  end)
  return id
end

copilot.rpc_request = function(client_id, method, params)
  local client = vim.lsp.get_client_by_id(client_id)
  if not client then return end
  local _, id
  _, id = client.rpc.request(method, params, function(err, result)
    vim.call('copilot#agent#LspResponse', client_id, {id = id, error = err, result = result})
  end)
  return id
end

copilot.rpc_notify = function(client_id, method, params)
  local client = vim.lsp.get_client_by_id(client_id)
  if not client then return end
  return client.rpc.notify(method, params)
end

return copilot
