let s:Buffer = vital#easymotion#import('Vim.Buffer')

function! EasyMotion#undo#save() abort
  return s:undo_lock.save()
endfunction

let s:undo_lock = {}

function! s:undo_lock.save() abort
  let undo = deepcopy(self)
  call undo._save()
  return undo
endfunction

function! s:undo_lock._save() abort
  if undotree().seq_last == 0
    " if there are no undo history, disable undo feature by setting
    " 'undolevels' to -1 and restore it.
    let self.save_undolevels = &l:undolevels
    let &l:undolevels = -1
  elseif !s:Buffer.is_cmdwin()
    " command line window doesn't support :wundo.
    let self.undofile = tempname()
    execute 'wundo!' self.undofile
  else
    let self.is_cmdwin = s:TRUE
  endif
endfunction

function! s:undo_lock.restore() abort
  if has_key(self, 'save_undolevels')
    let &l:undolevels = self.save_undolevels
  endif
  if has_key(self, 'undofile') && filereadable(self.undofile)
    silent execute 'rundo' self.undofile
    call delete(self.undofile)
  endif
  if has_key(self, 'is_cmdwin')
    " XXX: it breaks undo history. AFAIK, there are no way to save and restore
    " undo history in commandline window.
    call self.undobreak()
  endif
endfunction

function! s:undo_lock.undobreak() abort
  let old_undolevels = &l:undolevels
  setlocal undolevels=-1
  keepjumps call setline('.', getline('.'))
  let &l:undolevels = old_undolevels
endfunction
