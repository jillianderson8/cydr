# **************************************************************************** #
# Copyright (C) 2016 Jillian Anderson                                          #
# This file is part of the cydr package developed by Jillian Anderson during   #
# her 4th year Knowledge Integration Honours thesis at the University of       #
# Waterloo.                                                                    #
#                                                                              #
# cydr is free software: you can redistribute it and/or modify it under the    #
# terms of a GNU General Public License as published by the Free Software      #
# Foundation. cydr is distributed in the hope that it will be useful, but      #
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY   #
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for #
# more details.                                                                #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# cydr. If not, see <http://www.gnu.org/licenses/>.                            #
# **************************************************************************** #

#' Identifies data points whose Yld_Vol_Dr value is more than a specified
#' number of deviations from the mean.
#'
#' @param data a dataframe.
#' @param remove a boolean. Defaults to FALSE.
#' @param num_sd a integer in the range of [1, 4]. Defaults to 2.
#' @return A dataframe. Added column called cydr_Error. If a potential error
#' has been identified this column is TRUE. Otherwise it is FALSE.
#' If remove == TRUE, any observation whose cydr_Error attribute is TRUE is
#' removed.
#' @examples
#' id_residuals(data)
#' id_residuals(data, FALSE)
#' id_residuals(data, TRUE)
id_residuals <- function(data, remove=FALSE, num_sd=2){
  std_dev <- sd(data$Yld_Vol_Dr, na.rm=TRUE)
  meann <- mean(data$Yld_Vol_Dr, na.rm=TRUE)

  exist <- is.null(data$cydr_Error)

  if (exist)
    data_errors <- data %>%
      mutate(cydr_Error = Yld_Vol_Dr < (meann - num_sd*std_dev) |
                          Yld_Vol_Dr > (meann + num_sd*std_dev) |
                          cydr_Error)
  else
    data_errors <- data %>%
      mutate(cydr_Error = Yld_Vol_Dr < (meann - num_sd*std_dev) |
                          Yld_Vol_Dr > (meann + num_sd*std_dev))


  if (remove)
    data_errors <- data_errors %>%
      filter(is.na(cydr_Error) | !cydr_Error)

  return(data_errors)
}
