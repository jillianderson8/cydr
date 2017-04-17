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

#' Identify narrow passes
#' 
#' @description Adds a column called \code{cydr_NarrowPassError} to a dataframe to 
#' identify observations associated with narrow passes. 
#' 
#' Will identify all observations within narrow passes as erroneous. A pass is deemed
#' narrow if it has an average yield less than 1-\code{diff} times that of either its
#' neighbours.
#' 
#' @usage narrow_passes(data, remove=FALSE, passColumn=NULL, diff=0.15)
#' 
#' @param data a dataframe standardized and outputted from AgLeader.
#' @param remove a boolean. Defaults to \code{FALSE}. Indicates whether to remove
#' identified errors. 
#' @param passColumn a string or \code{NULL}. Defaults to \code{NULL}. Indicates what
#' column specifies an observation's pass number. If \code{NULL} the built-in helper
#' function \code{number_passes()} will be used to assign pass numbers to each observation. 
#' @param diff a numeric value between 0 and 1. Defaults to 0.15. Determines the allowable relative
#' difference between neighbouring passes. 
#' @return A dataframe with an added column called \code{cydr_NarrowPassError}. This column
#' will be set to \code{TRUE} if it meets the criteria for an erroneous observation.
#' @examples
#' narrow_passes(data)
#' narrow_passes(data, remove=TRUE, diff=0.25) # Allows differences of up to 25%
#' narrow_passes(data, passColumn="Pass_Num") # Will use Pass_Num to number passes as th
#' 
#' @family core functions
#' @export
narrow_passes <- function(data, remove=FALSE, passColumn=NULL, diff = 0.15){
  if (is.null(passColumn)){
    # If no column name has been supplied, use the number_passes helper 
    # to group observations. Place pass numbers in cydr_PassNum column
    numbered_passes <- number_passes(data)
  } else {
    # Use the values in passColumn as the pass numbers
    numbered_passes <- data %>%
      mutate("cydr_PassNum" = data$passColumn)
  }

  # Group observations by pass, find the average yield, 
  # and compare with neighbouring passes
  summ_data <- numbered_passes %>%
    group_by(cydr_PassNum) %>%
    summarise(avg_Yield = mean(Yld_Vol_Dr)) %>%
    mutate("cydr_NarrowPassError" = !!((avg_Yield/lead(avg_Yield)) < 1-diff | 
                                       (avg_Yield/lag(avg_Yield)) < 1-diff) &
                                    !is.na((avg_Yield/lead(avg_Yield)) < 1-diff | 
                                             (avg_Yield/lag(avg_Yield)) < 1-diff))
  
  # Create the data to be returned, add the error column
  ret_data <- merge(numbered_passes, summ_data)
  
  if(remove){
    # Remove observations identified as erroneous 
    ret_data <- ret_data %>%
    filter(is.na(cydr_Narrow_Pass) | !cydr_Narrow_Pass)
  }
  
  # Return the resulting dataframe
  return(ret_data)
}