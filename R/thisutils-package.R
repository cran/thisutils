# -*- coding: utf-8 -*-

#' @title Collection of Utility Functions for Data Analysis and Computing
#'
#' @useDynLib thisutils
#'
#' @description
#' Provides utility functions for data analysis and scientific computing. Includes functions for parallel processing, and other computational tasks to streamline workflows.
#'
#' @author Meng Xu (Maintainer), \email{mengxu98@qq.com}
#'
#' @source \url{https://mengxu98.github.io/thisutils/}
#'
#' @md
#' @docType package
#' @name thisutils-package
"_PACKAGE"

#' @title The logo of thisutils
#'
#' @description
#' The thisutils logo, using ASCII or Unicode characters
#' Use [cli::ansi_strip] to get rid of the colors.
#'
#' @md
#' @param unicode Unicode symbols on UTF-8 platforms.
#' Default is [cli::is_utf8_output].
#'
#' @references
#' \url{https://github.com/tidyverse/tidyverse/blob/main/R/logo.R}
#'
#' @export
#' @examples
#' thisutils_logo()
thisutils_logo <- function(
    unicode = cli::is_utf8_output()) {
  logo <- c(
    "    0        1      2           3    4
   __  __    _              __  _  __
  / /_/ /_  (_)_____ __  __/ /_(_)/ /_____
 / __/ __ ./ // ___// / / / __/ // // ___/
/ /_/ / / / /(__  )/ /_/ / /_/ // /(__  )
.__/_/ /_/_//____/ .__,_/.__/_//_//____/
  5             6      7      8       9"
  )

  hexa <- c("*", ".", "o", "*", ".", "o", "*", ".", "o", "*")
  if (unicode) {
    hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]
  }

  cols <- c(
    "red", "yellow", "green", "magenta", "cyan",
    "yellow", "green", "white", "magenta", "cyan"
  )

  col_hexa <- purrr::map2(
    hexa, cols, ~ cli::make_ansi_style(.y)(.x)
  )

  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  structure(
    cli::col_blue(logo),
    class = "thisutils_logo"
  )
}

#' @title Print logo
#'
#' @param x Input information.
#' @param ... Other parameters.
#'
#' @return Print the ASCII logo
#'
#' @method print thisutils_logo
#'
#' @export
#'
print.thisutils_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}

.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")

  msg <- paste0(
    strrep("-", 60),
    "\n",
    cli::col_blue(pkgname, " version ", version),
    "\n",
    cli::col_grey("This message can be suppressed by:"),
    "\n",
    cli::col_grey("  suppressPackageStartupMessages(library(thisutils))"),
    "\n",
    strrep("-", 60)
  )

  packageStartupMessage(thisutils_logo())
  packageStartupMessage(msg)
}
