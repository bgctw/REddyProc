#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Template of a function for automated documentation generation with the 'inlinedocs' package
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fTemplateInlinedocs<- function(
  ##title<<
  ## Short function title (mandatory)
  ##description<<
  ## Description of the function (optional)
  Var1.V.n              ##<< Description of input variable 1 (mandatory)
  ,Var2.V.n             ##<< Description of input variable 2 (mandatory)
)
  ##author<<
  ## Name initials of author(s) (mandatory)
  ##details<<
  ## Long and detailed description of the function (optional)
  ##seealso<<
  ## List of names of other functions doing similar stuff (optional)
  ## with links to documentation pages: \code{\link{fTemplateInlinedocs}}
  ##references<<
  ## List of papers describing the mechanisms or algorithms used (optional)
# Standard comments (with one hash): These will not appear in the documentation (optional)
# TODO(username): Explicit description of action to be taken (optional)
# TEST: Function arguments used for testing only (optional)
{
  ##details<<
  ## Description of what this part of your function does (optional)
  plot(Var1.V.n, Var2.V.n)
  
  lSum.V.n <- Var1.V.n + Var1.V.n
  
  ##details<<
  ## Further descriptions, e.g of lists (for lists use 'describe'):
  ##describe<<
  List.L <- list(     
    item0               ##<< Description of list item 0
    ,item1 = list(      ##<< A list inside a list:
      ##describe<< 
      # description of lists etc.
      ,item1.1=lSum.V.n       ##<< Description of list item 1.1
      ,item1.2=Var1.V.n       ##<< Description of list item 1.2
      ##end<<             
      # necessary to end enclosed 'describe'
    )
    ,item2              ##<< Description of item 2
  )
  
  lSum.V.n
  ##value<<
  ## Description of the return/output values of your function (mandatory)
}
attr(fTemplateInlinedocs,"ex") <- function(){
  # Example code demonstrating the functionality of your function
  x.V.n <- 1:10
  fTemplateInlinedocs(x.V.n,x.V.n*2)
}
