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
#' id_narrow_pass(data)
#' id_narrow_pass(data, FALSE)
#' id_narrow_pass(data, TRUE)
#'
#'
id_narrow_pass <- function(data, remove=FALSE, passes='cydr'){
  if (passes == 'cydr'){
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


#' One Line Description
#'
#' @param data describe parameter
#' @return value what does it return?
#' @examples
#' number_passes(data)
#'
#'
number_passes <- function(data){
  # Setting up variables
  len <- length(row.names(data))
  pos <- 1
  passNum <- 1
  passList <- c()
  OnTurn <- 0
  # Values for the progress bar

  while(pos <= len){
    
    update_progress_bar(pos, len)
    
    if (pos <= 5 | pos > len-5){
      # Check just if its a long turn
      # isTurn <- is_LongTurn # Actually a turn needs to be both, so just continue
      
    } else if (pos <= 20 | pos > len-20){
      # Check just if its a short turn
      # isTurn <- is_ShortTurn # Actually a turn needs to be both, so just continue
      
    } else {
      
      # Check if it is a short or a long turn
      Short_Old <- unlist(data[pos-5,]['Track_deg_'])
      Short_New <- unlist(data[pos+5,]['Track_deg_'])
      Long_Old  <- unlist(data[pos-10,]['Track_deg_'])
      Long_New  <- unlist(data[pos+10,]['Track_deg_'])
      
      #    isTurn <- is_ShortTurn(Short_Old, Short_New) & 
      #              is_LongTurn(Long_Old, Long_New)
      
      isTurn <- is_LongTurn(Long_Old, Long_New)
      
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