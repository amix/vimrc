let s:ABSPATH = '^/'
let s:RELPATH = '\v^\.\.?(/|$)'
let s:MODULE = '\v^(/|\.\.?(/|$))@!'

" Damn Netrw can't handle HTTPS at all. It's 2013! Insecure bastard!
let s:CORE_URL_PREFIX = "http://rawgit.com/joyent/node"
let s:CORE_MODULES = ["_debugger", "_http_agent", "_http_client",
	\ "_http_common", "_http_incoming", "_http_outgoing", "_http_server",
	\ "_linklist", "_stream_duplex", "_stream_passthrough", "_stream_readable",
	\ "_stream_transform", "_stream_writable", "_tls_legacy", "_tls_wrap",
	\ "assert", "buffer", "child_process", "cluster", "console", "constants",
	\ "crypto", "dgram", "dns", "domain", "events", "freelist", "fs", "http",
	\ "https", "module", "net", "node", "os", "path", "punycode", "querystring",
	\ "readline", "repl", "smalloc", "stream", "string_decoder", "sys",
	\ "timers", "tls", "tty", "url", "util", "vm", "zlib"]

function! node#lib#find(name, from)
	if index(s:CORE_MODULES, a:name) != -1
		let l:version = node#lib#version()
		let l:version = empty(l:version) ? "master" : "v" . l:version
		let l:dir = a:name == "node" ? "src" : "lib"
		return s:CORE_URL_PREFIX ."/". l:version ."/". l:dir ."/". a:name .".js"
	endif

	return s:resolve(s:absolutize(a:name, a:from))
endfunction

function! node#lib#version()
	if exists("b:node_version") | return b:node_version | endif
	if !executable("node") | let b:node_version = "" | return | endif
	let b:node_version = matchstr(system("node --version"), '^v\?\zs[0-9.]\+')
	return b:node_version
endfunction

function! s:absolutize(name, from)
	if a:name =~# s:ABSPATH
		return a:name
	elseif a:name =~# s:RELPATH
		let dir = isdirectory(a:from) ? a:from : fnamemodify(a:from, ":h")
		return dir . "/" . a:name
	else
		return b:node_root . "/node_modules/" . a:name
	endif
endfunction

function! s:resolve(path)
	" Node checks for files *before* directories, so see if the path does not
	" end with a slash or dots and try to match it as a file.
	if a:path !~# '\v/(\.\.?/?)?$'
		let path_with_suffix = s:resolveSuffix(a:path)
		if !empty(path_with_suffix) | return path_with_suffix | endif
	endif

	if isdirectory(a:path) | return s:resolveFromDirectory(a:path) | endif
endfunction

function! s:resolveFromDirectory(path)
	" Node.js checks for package.json in every directory, not just the
	" module's parent. According to:
	" http://nodejs.org/api/modules.html#modules_all_together
	if filereadable(a:path . "/package.json")
		" Turns out, even though Node says it does not support directories in
		" main, it does.
		" NOTE: If package.json's main is empty or refers to a non-existent file,
		" ./index.js is still tried.
		let main = s:mainFromPackage(a:path . "/package.json")

		if !empty(main) && main != ""
			let path = s:resolve(a:path . "/" . main)
			if !empty(path) | return path | endif
		endif
	endif

	" We need to check for ./index.js's existence here rather than leave it to
	" the caller, because otherwise we can't distinguish if this ./index was
	" from the directory defaulting to ./index.js or it was the package.json
	" which referred to ./index, which in itself could mean both ./index.js and
	" ./index/index.js.
	return s:resolveSuffix(a:path . "/index")
endfunction

function! s:mainFromPackage(path)
	for line in readfile(a:path)
		if line !~# '"main"\s*:' | continue | endif
		return matchstr(line, '"main"\s*:\s*"\zs[^"]\+\ze"')
	endfor
endfunction

function! s:resolveSuffix(path)
	for suffix in s:uniq([""] + g:node#suffixesadd + split(&l:suffixesadd, ","))
		let path = a:path . suffix
		if filereadable(path) | return path | endif
	endfor
endfunction

let s:GLOB_WILDIGNORE = 1

function! node#lib#glob(name)
	let matches = []

	if empty(a:name)
		let matches += s:CORE_MODULES
	endif

	if empty(a:name) || a:name =~# s:MODULE
		let root = b:node_root . "/node_modules"
		let matches += s:glob(empty(a:name) ? root : root . "/" . a:name, root)
	endif

	if a:name =~# s:ABSPATH
		let matches += s:glob(a:name, 0)
	endif

	if empty(a:name) || a:name =~# s:RELPATH
		let root = b:node_root
		let relatives = s:glob(empty(a:name) ? root : root . "/" . a:name, root)

		"call map(relatives, "substitute(v:val, '^\./\./', './', '')")
		if empty(a:name) | call map(relatives, "'./' . v:val") | endif
		call filter(relatives, "v:val !~# '^\\.//*node_modules/$'")

		let matches += relatives
	endif

	return matches
endfunction

function! s:glob(path, stripPrefix)
	" Remove a single trailing slash because we're adding one with the glob.
	let path = substitute(a:path, '/$', "", "")
	" Glob() got the ability to return a list only in Vim 7.3.465. Using split
	" for compatibility.
	let list = split(glob(fnameescape(path)."/*", s:GLOB_WILDIGNORE), "\n")

	" Add slashes to directories, like /bin/ls.
	call map(list, "v:val . (isdirectory(v:val) ? '/' : '')")

	if !empty(a:stripPrefix)
		" Counting and removing bytes intentionally as there's no substr function
		" that takes character count, only bytes.
		let	prefix_length = len(a:stripPrefix) + 1
		return map(list, "strpart(v:val, prefix_length)")
	endif

	return list
endfunction

function! s:uniq(list)
	let list = reverse(copy(a:list))
	return reverse(filter(list, "index(list, v:val, v:key + 1) == -1"))
endfunction
