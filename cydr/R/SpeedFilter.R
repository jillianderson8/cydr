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

#' Identify outlying speeds
#' 
#' @description Adds a column called \code{cydr_SpeedError} to a dataframe to identify
#' observations associated with outlying speeds. 
#' 
#' Will identify all observations with speeds greater than \code{sd} standard
#' deviations from the mean. If \code{remove} is \code{TRUE} all erroneous 
#' observations will be removed from the dataframe. 
#' 
#' @usage speed(data, remove=FALSE, type="both", sd=2)
#' @param data a dataframe standardized and outputted from AgLeader.
#' @param remove a boolean. Defaults to \code{FALSE}. Indicates whether to remove
#' identified errors. 
#' @param type one of \code{"high"}, \code{"low"}, or \code{"both"} to determine 
#' which types of data to identify as erroneous.  \code{"high"} will identify 
#' fast speeds, \code{"low"} will identify slow speeds, and \code{"both"} will 
#' identify data associated with either fast or slow speeds.
#' @param sd a number >= 0. Defaults to 2. Used as the standard deviation 
#' threshold for error identification.
#' @return A dataframe with an added column called \code{cydr_SpeedError}. This column
#' will be set to \code{TRUE} if it meets the criteria for an erroneous observation.
#' 
#' If \code{remove = TRUE} all observations cydr identifies as erroneous are 
#' removed from the returned dataframe. 
#' @examples
#' speed(data)
#' speed(data, type="both")
#' speed(data, FALSE, type="low")
#' speed(data, TRUE, type="high")
#' 
#' @family core functions
#' 
speed <- function(data, remove=FALSE, type="both", sd = 2){
  # Compute the standard deviation of speed
  std_dev <- sd(data$Speed_mph_, na.rm=TRUE)
  # Compute the mean speed
  meann <- mean(data$Speed_mph_, na.rm=TRUE)
  
  if(type=="both"){
    # Identify high and low speeds
    data_errors <- data %>%
      mutate(cydr_SpeedError = Speed_mph_ < (meann - sd*std_dev) |
                               Speed_mph_ > (meann + sd*std_dev))
  } else if (type=="low"){
    # Identify low speeds
    data_errors <- data %>%
      mutate(cydr_SpeedError = Speed_mph_ < (meann - sd*std_dev))
  } else if (type=="high"){
    # Identify high speeds
    data_errors <- data %>%
      mutate(cydr_SpeedError = Speed_mph_ > (meann + sd*std_dev))
  }
  
  if (remove) {
    # Filter out observations identified as speed errors
    data_errors <- data_errors %>%
    filter(is.na(cydr_SpeedError) | !cydr_SpeedError)
  }
  
  # Return the final dataframe with added column cydr_SpeedError
  return(data_errors)
}