function Goal (replace)

  " Save this file and call the ghc-goals tool in the background with our
  " module and word-under-cursor as arguments.
  silent w
  let cmd    = "ghc-goals " . shellescape(expand("%")) . " -g " . shellescape(expand("<cword>"))
  let output = system(cmd)

  " Join multi-line type signatures.
  let output = substitute(output, "\n\\s\\+", " ", "g")

  " Filter everything that is not related to the current line.
  let list   = split(output, "\n")
  let list   = filter(list, "v:val =~ " . line("."))
  let output = join(list, " ")

  " Filter out the comment from the signature.
  let output = substitute(output, "\\s*--.*", "", "g")

  " Print out the type.
  echomsg output

  if a:replace
    exe "normal ciw(" . output . ")\<esc>"
  endif

endfunction


" Install goal collector for Haskell and literate Haskell files.

function Install ()
  map <buffer> <F4> :call Goal(0)<CR> 
  map <buffer> <F5> :call Goal(1)<CR> 
endfunction

autocmd BufRead,BufNewFile *.{hs,lhs} call Install()

