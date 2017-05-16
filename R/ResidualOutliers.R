# **************************************************************************** #
# Copyright (C) 2017 Jillian Anderson                                          #
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

#' Identify outlying yields
#' 
#' @description Adds a column called \code{cydr_ResidualError} to a 
#' dataframe to identify observations associated with outlying yields. 
#' 
#' Will identify all observations with yields greater than \code{sd} standard
#' deviations away from the mean. If \code{remove} is \code{TRUE} all 
#' observations associated with a residual error will be removed from the 
#' dataframe. 
#' 
#' @usage residual_outliers(data, remove=FALSE, type="both", sd=2)
#' 
#' @param data a dataframe, standardized and outputted from AgLeader.
#' @param remove a boolean. Defaults to \code{FALSE}. Indicates whether to remove
#' identified errors. 
#' @param type one of \code{"high"}, \code{"low"}, or \code{"both"}. Indicates
#' which types of data to identify as erroneous \code{"high"} will identify, 
#' high yields, \code{"low"} will identify low speeds, and \code{"both"} will
#' identify high and low speeds. 
#' @param sd a number >= 0. Defaults to 2. Used as the standard deviation 
#' threshold for error identification.
#' @return A dataframe with an added column called \code{cydr_ResidualError}. 
#' This column will be set to \code{TRUE} if an observation is deemed erroneous.
#' 
#' If \code{remove = TRUE} all observations cydr identifies as erroneous are
#' removed from the returned dataframe. 
#' @examples
#' residual_outliers(data)
#' residual_outliers(data, TRUE)
#' residual_outliers(data, TRUE, type="low", sd=3)
#' 
#' @family core functions
#' @export
residual_outliers <- function(data, remove=FALSE, type="both", sd=2){
  # Compute the standard deviation of yield
  std_dev <- sd(data$Yld_Vol_Dr, na.rm=TRUE)
  # Compute the mean yield
  meann <- mean(data$Yld_Vol_Dr, na.rm=TRUE)

  if (type=="both"){
    # Identify high and low yielding observations
    data_errors <- data %>%
      mutate(cydr_ResidualError = Yld_Vol_Dr < (meann - sd*std_dev) |
                                   Yld_Vol_Dr > (meann + sd*std_dev))
  } else if (type=="low"){
    # Identify low yielding observations
    data_errors <- data %>%
      mutate(cydr_ResidualError = Yld_Vol_Dr < (meann - sd*std_dev))
  } else if (type == "high"){
    # Identify high yielding observations
    data_errors <- data %>%
      mutate(cydr_ResidualError = Yld_Vol_Dr > (meann + sd*std_dev))
  }

  if (remove) {
    # Filter out observations identified as residual errors
    data_errors <- data_errors %>%
      filter(is.na(cydr_ResidualError) | !cydr_ResidualError)
  }
  return(data_errors)
}
