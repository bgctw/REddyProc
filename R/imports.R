#' @import methods
NULL

# #' @importFrom Rcpp sourceCpp evalCpp
#' @import dplyr
#' @importFrom purrr map map_df map_dbl map_lgl pmap
#' @importFrom tibble tibble as_tibble tribble
# #' @importFrom mlegp mlegp predict.gp
#' @importFrom rlang UQ sym syms
#' @importFrom readr col_character col_double col_guess col_factor cols cols_only
#'        read_csv write_csv read_lines
NULL

#' @importFrom grDevices adjustcolor colorRampPalette dev.off pdf png rgb
#'        devAskNewPage
#' @importFrom graphics abline axis box close.screen curve image legend lines
#'        mtext par plot points polygon screen split.screen
#' @importFrom stats aggregate anova approx coef cor cov lm median na.omit
#'        nls nls.control optim predict quantile resid rnorm sd setNames
#' @importFrom utils capture.output download.file read.csv recover write.table
#'        warnErrList
#' @importFrom stats plogis qlogis
NULL

#' @import Rcpp
#' @useDynLib REddyProc _REddyProc_whichValueGreaterEqualC
NULL

# only checked in code by require(), let R CMD check complain (only NOTE)
#importFrom(boot, boot)
#importFrom(segmented, segmented, seg.control)
#importFrom(minpack.lm, nlsLM, nls.lm.control)


#' @importFrom bigleaf Esat.slope
NULL

# https://www.r-bloggers.com/re-exporting-the-magrittr-pipe-operator/

# does not work without rd roclet
#' re-export magrittr pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL
#"%>%" <- magrittr::"%>%"

NULL
