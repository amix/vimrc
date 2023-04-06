local module = {}

local ale_type_to_diagnostic_severity = {
  E = vim.diagnostic.severity.ERROR,
  W = vim.diagnostic.severity.WARN,
  I = vim.diagnostic.severity.INFO
}

module.sendAleResultsToDiagnostics = function(buffer, loclist)
  local diagnostics = {}

  -- Convert all the ALE loclist items to the shape that Neovim's diagnostic
  -- API is expecting.
  for _, location in ipairs(loclist) do
    table.insert(
      diagnostics,
      -- All line numbers from ALE are 1-indexed, but all line numbers
      -- in the diagnostics API are 0-indexed, so we have to subtract 1
      -- to make this work.
      {
        lnum = location.lnum - 1,
        -- Ending line number, or if we don't have one, just make it the same
        -- as the starting line number
        end_lnum = (location.end_lnum or location.lnum) - 1,
        -- Which column does the error start on?
        col = math.max((location.col or 1) - 1, 0),
        -- end_col does *not* appear to need 1 subtracted, so we don't.
        end_col = location.end_col,
        -- Which severity: error, warning, or info?
        severity = ale_type_to_diagnostic_severity[location.type] or "E",
        -- The error message
        message = location.text,
        -- e.g. "rubocop"
        source = location.linter_name,
      }
    )
  end

  local virtualtext_enabled_set = {['all'] = true, ['2'] = true, [2] = true, ['current'] = true, ['1'] = true, [1] = true}

  vim.diagnostic.set(
    vim.api.nvim_create_namespace('ale'),
    buffer,
    diagnostics,
    { virtual_text = virtualtext_enabled_set[vim.g.ale_virtualtext_cursor] ~= nil}
  )
end

return module
