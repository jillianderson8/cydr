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

#' Identify Narrow Passes in a dataframe
#' @description Identify narrow passes within a data frame representing a field's
#' yield. It creates a new data frame with an additional column to flag narrow
#' passes. This is particularily useful in conjunction with the other cydr 
#' identification functions to identify erroneous observations in agricultural 
#' yield data. 
#' @param data a dataframe.
#' @param remove a boolean. Defaults to FALSE
#' @param passes a string, either 'cydr' or the column in the data which specifies
#' an observations pass number. Defaults to 'cydr'.
#' @return A dataframe. Added column called cydr_Narrow_Pass. If a point's pass has
#' been identified as narrow this column is TRUE. Otherwise it is FALSE. 
#' @examples
#' narrow_pass(data)
#' narrow_pass(data, FALSE)
#' narrow_pass(data, TRUE)
#'
#'
narrow_pass <- function(data, remove=FALSE, passes='cydr_PassNum'){
  if (passes == 'cydr_PassNum'){
    numbered_passes <- number_passes(data)
  } else {
    
  }

  summ_data <- numbered_passes %>%
    group_by(cydr_PassNum) %>%
    summarise(avg_Yield = mean(Yld_Vol_Dr)) %>%
    mutate("cydr_Narrow_Pass" = (avg_Yield/lead(avg_Yield)*100) < 85 | 
                                (avg_Yield/lag(avg_Yield)*100) <85)
  
  ret_data <- merge(numbered_passes, summ_data)
  
  if(remove){
    ret_data <- ret_data %>%
    filter(is.na(cydr_Narrow_Pass) | !cydr_Narrow_Pass)
  }
  
  return(ret_data)
}


#' Determines pass numbers in a dataframe
#'
#' @param data describe parameter
#' @return a new dataframe containing a new column called cydr_PassNum 
#' representing 
#'
#' @examples
#' number_passes(data)
#'
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
      ST <- is_ShortTurn(Short_Old, Short_New)
      
      # Check if point is part of long turn
      Long_Old  <- unlist(data[pos-10,]['Track_deg_'])
      Long_New  <- unlist(data[pos+10,]['Track_deg_'])
      LT <- is_LongTurn(Long_Old, Long_New)
      
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


#' One Line Description
#'
#' @param lag describe parameter
#' @param lead describe parameter
#' @param threshold describe parameter
#' @return value what does it return?
#' @examples
#' is_ShortTurn(lead, lag, threshold)
#' @seealso is_LongTurn
#'
is_ShortTurn <- function(lag, lead, threshold=45){
  dirDiff <- calc_direction_change(lag, lead)
  return(dirDiff > threshold)
}

#' One Line Description
#'
#' @param lag describe parameter
#' @param lead describe parameter
#' @param threshold describe parameter
#' @return value what does it return?
#' @examples
#' is_LongTurn(data)
#' @seealso is_ShortTurn
#'
is_LongTurn <- function(lag, lead, threshold=100){
  dirDiff <- calc_direction_change(lag, lead)
  return(dirDiff > threshold)
}

#' One Line Description
#'
#' @param lag describe parameter
#' @param lead describe parameter
#' @param threshold describe parameter
#' @return value what does it return?
#' @examples
#' update_progress_bar(pos, len)
#'
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


#' One Line Description
#'
#' @param lag describe parameter
#' @param lead describe parameter
#' @param threshold describe parameter
#' @return value what does it return?
#' @examples
#' update_progress_bar(pos, len)
#'
plot_cydr_Passes <- function(data){
  ggplot(data, aes(coords.x1-(Swth_Wdth_ * 0.3048 / 111111 / cos(coords.x2)), 
                   coords.x2, 
                   xend=coords.x1+(Swth_Wdth_ * 0.3048 / 111111 / cos(coords.x2)), 
                   yend=coords.x2, 
                   alpha=0.5,
                   colour=cydr_Narrow_Pass)) +
    geom_segment()
}