function Goal ()

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
  let output = substitute(output, "--.*", "", "g")

  " Print out the type.
  echomsg output

endfunction



" Install goal collector for Haskell and literate Haskell files.

function Install ()
  map <buffer> <F4> :call Goal()<CR> 
endfunction

autocmd BufRead,BufNewFile *.{hs,lhs} call Install()

