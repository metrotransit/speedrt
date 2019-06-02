# This file is part of speedRT
# Copyright (c) 2019 Metropolitan Council
#
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.

#' Convert character time to ITime, 28 hour clock
#'
#' @param x time represented as a character string
#'
#' @return seconds from midnight with ITime class, service continuing past
#' midnight will continue to count seconds from previous day.
#' @export
#'
#' @examples
#' as.TransitTime(c('1:00', '25:00'))
as.TransitTime <- function(x) {
  y <- as.ITime(x)
  ix <- is.na(y)
  if (any(ix)) {
    yy <- x[ix]
    hrs <- as.integer(gsub('^(\\d{1,2}):.+', '\\1', yy))
    new_hrs <- hrs - 24
    not_hrs <- gsub('^(\\d{1,2})(:.+)', '\\2', yy)
    y[ix] <- as.ITime(paste0(new_hrs, not_hrs)) + 86400
  }
  y
}
