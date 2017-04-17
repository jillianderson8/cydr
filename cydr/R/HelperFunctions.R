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

#' Calculates difference in direction between two angles
#'
#' @description A helper function used by `short_turn` and `long_turn`
#'to determine the angular difference in degrees between two directions.
#' 
#' @usage calc_direction_change(oldDir, newDir)
#' 
#' @param oldDir a number between 0 and 360
#' @param newDir a number between 0 and 360
#' @return The difference between two angles. A numeric value between -180 and 180. 
#' @examples
#' calc_direction_change(20, 180) # => 160
#' calc_direction_change(350, 1)  # => 11
#' calc_direction_change(180, 0)  # => -180
#' calc_direction_change(180, 50) # => -130
#'
#' @seealso short_turn, long_turn
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


#' Determines whether a point is within a long turn
#'
#' @description a helper function used by `number_passes` and 
#' `pass_end_turns`` to determine whether a point lies within 
#'  a long turn.
#'  
#'  If the difference between the lag and lead directions is greater
#'  than the provided threshold the function returns `TRUE` and a 
#'  long turn is identified.  
#'  
#' @usage long_turn(lag, lead, threshold=100)
#' 
#' @param lag a number between 0 and 360. The first direction
#' being compared by the function. 
#' @param lead a number between 0 and 360. The second direction
#' being compared by the function.
#' @param threshold a number between 0 and 180. Corresponds to the 
#' highest difference in direction which is considered not a turn.
#' @return boolean. `TRUE`  if the difference between directions is
#' greater than `threshold`, `FALSE` otherwise.
#' @examples
#' long_turn(100, 0) # => FALSE
#' long_turn(10, 200)  # => TRUE
#' long_turn(360, 0) # => FALSE
#' long_turn(0, 180, threshold=178) # => TRUE
#'  
#' @seealso short_turn, number_passes, narrow_passes
#'
long_turn <- function(lag, lead, threshold=100){
  dirDiff <- calc_direction_change(lag, lead)
  return(abs(dirDiff) > threshold)
}


# First, the `number_passes()` helper function can be used to group a data frame's observations 
# into passes. This function is called when `PassNum = NULL` in the `narrow_passes()` function, which 
# is the function's default behavior. This function loops through the dataset and determines when the 
# end of a pass is reached and increments the pass which is recorded.

#' Groups a yield data frame into passes
#'
#' @description a helper function used by `narrow_passes` to group
#' observations into passes. Used if no `passColumn` is supplied to the
#' `narrow_passes` function. 
#' 
#'  Will loop through the dataset and assign pass numbers to each 
#'  observation. 
#'  
#' @usage number_passes(data)
#' 
#' @param data a data frame representing a set of yield data. The 
#' data frame must include a `Track_deg_` column.
#' @return a data frame with an added column called `cydr_PassNum`. 
#' This column stores the pass number of each observation. 
#' 
#' @seealso narrow_passes
#'
number_passes <- function(data){
  # Variable Initialization
  len <- length(row.names(data))  # number of rows in data
  pos <- 1                        # used to iterate through data
  passNum <- 1                    # stores current pass number
  passList <- c()                 # vector containing all point's associated pass number
  OnTurn <- 0                     # indicates whether the previous point was a turn (0->not Turn, 1->Turn)
  
  # Loop through data's rows, incremementing pos each time. 
  while(pos <= len){
    
    update_progress_bar(pos, len) # print progress to console every percent
    
    # Checks if the point is among the first or last 10. Skip calculations
    #   to prevent failure. Points are not considered as turns.
    if (pos <= 10 | pos > len-10){
      # If short turn, continue
    } else {
      # Check if point is part of a short turn
      Short_Old <- unlist(data[pos-5,]['Track_deg_'])
      Short_New <- unlist(data[pos+5,]['Track_deg_'])
      ST <- short_turn(Short_Old, Short_New)
      
      # Check if point is part of long turn
      Long_Old  <- unlist(data[pos-10,]['Track_deg_'])
      Long_New  <- unlist(data[pos+10,]['Track_deg_'])
      LT <- long_turn(Long_Old, Long_New)
      
      # Classify point as turn if part of both short and long turn
      isTurn <- ST & LT
      
      if (floor(OnTurn/1) & !isTurn){
        passNum <- passNum + 1
        OnTurn <- 0
      } else if (!floor(OnTurn/1) & isTurn) {
        # passNum <- passNum + 1
        OnTurn <- OnTurn + 1
      }
    }
    passList[pos] <- passNum
    pos <- pos + 1
  }
  
  newdata <- data
  newdata$cydr_PassNum <- passList
  
  return(newdata)
}


#' Determines whether a point is within a short turn
#'
#' @description a helper function used by `number_passes` and 
#' `pass_end_turns` to determine whether a point lies within 
#'  a short turn.
#'  
#'  If the difference between the lag and lead directions is greater
#'  than the provided threshold the function returns `TRUE` and a 
#'  long turn is identified.  
#'  
#' @usage short_turn(lag, lead, threshold=45)
#' 
#' @param lag a number between 0 and 360. The first direction
#' being compared by the function. 
#' @param lead a number between 0 and 360. The second direction
#' being compared by the function.
#' @param threshold a number between 0 and 180. Corresponds to the 
#' highest difference in direction which is considered not a turn.
#' @return boolean. `TRUE`  if the difference between directions is
#' greater than `threshold`, `FALSE` otherwise.
#' @examples
#' short_turn(100, 0) # => TRUE
#' short_turn(45,90)  # => FALSE
#' short_turn(360, 0) # => FALSE
#' short_turn(0, 45, threshold=60) # => FALSE
#' 
#' @seealso long_turn, number_passes, narrow_passes
#'
short_turn <- function(lag, lead, threshold=45){
  dirDiff <- calc_direction_change(lag, lead)
  return(abs(dirDiff) > threshold)
}

#' Updates a console progress bar
#'
#' @description A helper function used by `number_passes`. Used
#' to create and update a console progress bar. For each for each
#' additional percent through len pos gets, a '.' is printed to
#' the console. Every ten percent the current progress is printed
#' to the screen.
#' @param pos a numeric value greater than zero, such that pos<=len. 
#' Corresponds to the progress made through the function. 
#' @param len a numberic value greater than zero, such that pos<=len. 
#' @return NULL
#'
#' @seealso number_passes
update_progress_bar <- function(pos, len){
  percent <- floor(len / 100)
  percent10 <- percent * 10 
  
  if (pos %% percent10 == 0){
    cat(toString(floor(pos/percent10)*10))
    cat("%")
  } else if (pos %% percent == 0){
    cat(".")
  }
}