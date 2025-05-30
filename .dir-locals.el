(
 (haskell-mode
  .
  (
   (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans"
                                       "--no-build"
                                       "--no-load"
                                       "process-escape-windows:lib"
                                       "process-escape-windows:test:tests"
                                       ))
   )
  )
 )
