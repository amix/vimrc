" Author: w0rp <devw0rp@gmail.com>
" Description: Follow-up checks for the plugin: warn about conflicting plugins.

" A flag for ensuring that this is not run more than one time.
if exists('g:loaded_ale_after')
    finish
endif

" Set the flag so this file is not run more than one time.
let g:loaded_ale_after = 1

" Check if the flag is available and set to 0 to disable checking for and
" emitting conflicting plugin warnings.
if exists('g:ale_emit_conflict_warnings') && !g:ale_emit_conflict_warnings
    finish
endif

" Conflicting Plugins Checks

function! s:GetConflictingPluginWarning(plugin_name) abort
    return 'ALE conflicts with ' . a:plugin_name
    \   . '. Uninstall it, or disable this warning with '
    \   . '`let g:ale_emit_conflict_warnings = 0` in your vimrc file, '
    \   . '*before* plugins are loaded.'
endfunction

if exists('g:loaded_syntastic_plugin')
    throw s:GetConflictingPluginWarning('Syntastic')
endif

if exists('g:loaded_neomake')
    throw s:GetConflictingPluginWarning('Neomake')
endif

if exists('g:loaded_validator_plugin')
    throw s:GetConflictingPluginWarning('Validator')
endif
