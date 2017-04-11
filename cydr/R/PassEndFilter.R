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

#' A helper function for ____ which calculates difference between two angles.
#'
#' @param oldDir a number in the range of \[0, 360\)
#' @param newDir a number in the range of \[0, 360\)
#' @return The difference between two angles.
#' @examples
#' calc_direction_change(20, 180)
#' calc_direction_change(350, 1)
#' calc_direction_change(0, 180)
#'
calc_direction_change <- function(oldDir, newDir){
  diff <- newDir - oldDir
  if (is.na(diff)){
    return(NA_real_)
  }else if (diff < -180){
    diff = diff + 360
  } else if (diff > 180){
    diff = diff - 360
  }
  return(diff)
}

#' Identifies turns.
#' @description id_pass_turns identifies turns at the end of passes within a
#' field.
#' @param data a dataframe
#' @param remove a boolean. Defaults to FALSE.
#' @return A dataframe. Added column called cydr_Error. If a potential error
#' has been identified this column is TRUE. Otherwise it is FALSE.
#' @examples
#' id_pass_end(field1)
#' id_pass_end(field1, remove=TRUE)
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
    mutate(Short_Turn = is_ShortTurn(SOldDir, SFutDir, short_angle)) %>% #Determine if short turn
    mutate(Long_Turn = is_LongTurn(LOldDir, LFutDir, long_angle)) %>% #Determine if long turn
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
