function Goal ()
  silent w
  let cmd    = "ghc-goals " . shellescape(expand("%")) . " -g " . shellescape(expand("<cword>"))
  let output = system(cmd)
  let output = substitute(output, "\n\\s\\+", " ", "g")
  let list   = split(output, "\n")
  let list   = filter(list, "v:val =~ " . line("."))
  let output = join(list, " ")
  let output = substitute(output, "--.*", "", "g")
  echomsg output
endfunction

function Install ()
  map <buffer> <F4> :call Goal()<CR> 
endfunction

autocmd BufRead,BufNewFile *.{hs,lhs} call Install()

