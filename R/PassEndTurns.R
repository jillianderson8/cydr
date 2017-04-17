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

#' Identify pass-end turns
#' 
#' @description Adds a column called \code{cydr_PassEndError} to a dataframe to 
#' identify observations occurring within pass-end turns. These observations are
#' identified by comparing two pairs of points occurring before and after the point
#' of interest. If the difference in direction between both pairs of points is 
#' above their respective thresholds, the observation is identified as pass-end
#' turn error.
#'  
#' The first pair identifies whether the point is within a "short-turn", by default 
#' checking if the points 5 before and 5 after have a difference in direction equal
#' to or greater than 45 degrees. The second pair identifies whether the point is 
#' within a "long-turn", by default checking whether the points 20 before and 20 
#' after have a difference in direction of 178 degrees or greater.
#' 
#' The thresholds used to determine short- and long-turns can all be customized using
#' the provided arguments. 
#'  
#' @usage pass_end_turns(data, remove=FALSE, short_angle=45, long_angle=178, short_offset =5, long_offset=20)
#' 
#' @param data a dataframe standardized and outputted from AgLeader.
#' @param remove a boolean. Defaults to \code{FALSE}. Indicates whether to remove
#' identified errors. 
#' @param short_angle a number between 0 and 180. Defaults to 45. Used as the angle 
#' to determine whether an observation is within a short-turn. If the difference between 
#' compared points is greater than this angle, a short-turn is identified. 
#' @param long_angle a number between 0 and 180. Defaults to 178. Used as the angle 
#' to determine whether an observation is within a long-turn. If the difference between 
#' compared points is greater than this angle, a long-turn is identified.   
#' @param short_offset a number greater than 0 such that \code{short_offset < long_offset}.
#' Defaults to 5. Used to determine the pair of numbers which will be compared to determine 
#' whether a short-turn is occurring. For example, a \code{short_offset} of 5 will compare 
#' the point 5 prior and the point 5 past the point of interest. If the difference in these
#' points' directions is greater than \code{short_angle}, a short-turn is occurring.
#' @param long_offset a number greater than 0 such that \code{short_offset < long_offset}.
#' Defaults to 20. Used to determine the pair of numbers which will be compared to determine 
#' whether a long-turn is occurring. For example, a \code{long_offset} of 20 will compare 
#' the point 20 prior and the point 20 past the point of interest. If the difference in these
#' points' directions is greater than \code{long_angle}, a long-turn is occurring. 
#' @return A dataframe with an added column called \code{cydr_PassEndError}. This column
#' will be set to \code{TRUE} if it meets the criteria for an erroneous observation.
#' @examples
#' pass_end_turns(data)
#' pass_end_turns(data, remove=TRUE) # Removes all identified errors
#' pass_end_turns(data, long_angle=170) # Identifies differences in long_offset points 
#' of > 170 as erroneous. 
#' 
#' @family core functions
#' 
pass_end_turns <- function(data, remove=FALSE, short_angle=45, long_angle=178, 
                           short_offset =5, long_offset=20){
  
  # Determine whether an observation is part of both a short and long turn
  data_errors <- data %>%
    group_by(Dataset) %>% #Group by original dataset
    arrange(Obj__Id) %>%  #Order by occurence
    mutate(SOldDir = lag(Track_deg_, n = short_offset)) %>%  #Find values for short comparison
    mutate(SFutDir = lead(Track_deg_, n = short_offset)) %>%
    mutate(LOldDir = lag(Track_deg_, n = long_offset)) %>% #Find values for long comparison
    mutate(LFutDir = lead(Track_deg_, n = long_offset)) %>%
    rowwise() %>%
    mutate(Short_Turn = short_turn(SOldDir, SFutDir, short_angle)) %>% #Determine if short turn
    mutate(Long_Turn = long_turn(LOldDir, LFutDir, long_angle)) %>% #Determine if long turn
    mutate(cydr_PassEndError = ((Short_Turn & Long_Turn) | 
                                  (Short_Turn & is.na(Long_Turn)) | 
                                  (is.na(Short_Turn) & Long_Turn)) &
             !(is.na(Short_Turn) & is.na(Long_Turn)))
  
  # Return only one new column -- cydr_PassEndError
  retdata <- data %>%
    mutate (cydr_PassEndError = data_errors$cydr_PassEndError)
  
  if(remove)
    # Remove observations associated with pass-end turns
    retdata <- retdata %>%
    filter(!cydr_TurnErrors)
  
  # Return the dataframe with an added column
  return(retdata)
}
