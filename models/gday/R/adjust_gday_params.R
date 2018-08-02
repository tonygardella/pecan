##' @title adjust_gday_params
##' @export
##' @description R Function to edit param file for GDAY
##' @param in_fname location on disk where inputs are stored
##' @param out_fname prefix of input and output files
##' @param replacements location on disk where outputs will be stored
##' @return generates GDAY .ini param file in the out_fname path
##' @author Martin De Kauwe
##' \dontrun{
##' in_fname <- "base_start.cfg"
##' out_fname <- "test.ini"
##' replacements <- list("ncycle" = "true",
##'                     "modeljm" = "3",
##'                     "print_options" = "end",
##'                     "jmax" = "110.0",
##'                     "vcmax" = "55.0")
##'adjust_gday_params(in_fname, out_fname, replacements)
##' }
adjust_gday_params <- function(in_fname, out_fname, replacements) {
  
  g <- ini::read.ini(in_fname)
  
  for (key in names(replacements)) {
    
    match_git <- key %in% names(g$git)
    match_files <- key %in% names(g$files)
    match_params <- key %in% names(g$params)
    match_state <- key %in% names(g$state)
    match_control <- key %in% names(g$control)
    
    if (match_git) {
      g$git[key] <- replacements[key]
    } else if (match_files) {
      g$files[key] <- replacements[key]
    } else if (match_params) {
      g$params[key] <- replacements[key]
    } else if (match_state) {
      g$state[key] <- replacements[key]
    } else if (match_control) {
      g$control[key] <- replacements[key]
    }
    
  }
  write.ini(g, out_fname)
}
