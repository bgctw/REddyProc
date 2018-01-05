#' @export
donttest <- function(
  ### run a block of code only in interactive R sessions
  block                          ##<< block of code to be run or not
  ,run.donttest = interactive()  ##<< same as arguemnt in \code{\link{example}}.
    ## Logical scalar whether to evaluate the block of code.
    ## Defaults to TRUE in interactive sessions and FALSE otherwise.
){
  ##author<< Thomas Wutzler
  ##details<< Rd-files use the syntax \code{\\donttest{block}}
  ##, which is latex code (see \code{\link{example}}).
  ## In order to specify the same behaviour with valid R-code
  ##  this function can be used. This is necessary, if inlinedocs is used
  ##  to generate the Rd files.
  parentFrame = parent.frame()
  if (run.donttest) eval(block, envir = parentFrame) else NULL
  ##value<< return value of \code{eval(block)} or NULL if not evaluated.
}
