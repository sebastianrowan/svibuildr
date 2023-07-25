#' Return a social vulnerability index for a specified region according to the
#' CDC ATSDR methodology using the tidycensus package developed by Kyle Walker.
#'
#' This packages uses US Census Bureau data but is neither endorsed nor
#' supported by the US Census Bureau.
#'
#' This package used the SVI methodology published by the CDC Agency for
#' Toxic Substances and Disease Registry (ATSDR) but is neither endorsed nor
#' supported by the ADSTR.
#'
#' @author Sebastian Rowan
#' @name svibuildr
#' @docType package
#' @import cli
#' @import dplyr
#' @import sf
#' @import stringr
#' @import tidycensus
#' @import tidyselect
#' @import tigris
#' @import withr
#' @import readr
#' @importFrom magrittr `%>%`
#' @importFrom tidyr replace_na
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom rlang inform abort arg_match
NULL
