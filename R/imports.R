#' @import methods
NULL

#' @importFrom Rcpp sourceCpp evalCpp
#' @import dplyr
#' @importFrom purrr map map_df
#' @importFrom tibble tibble as_tibble
#' @importFrom mlegp mlegp predict.gp
#' @importFrom logitnorm logit invlogit
NULL

#' @useDynLib REddyProc _REddyProc_whichValueGreaterEqualC
NULL

# only checked in code by require(), let R CMD check complain (only NOTE)
#importFrom(boot, boot)
#importFrom(segmented, segmented, seg.control)
#importFrom( minpack.lm, nlsLM, nls.lm.control )

