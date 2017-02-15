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

#' One Line Description
#'
#' @param data describe parameter
#' @param remove describe parameter
#' @param type describe parameter
#' @return value what does it return?
#' @examples
#' id_speed(data, type="both")
#' id_speed(data, FALSE, type="low")
#' id_speed(data, TRUE, type="high")
#'
#'
id_speed <- function(data, remove=FALSE, type="both"){
  std_dev <- sd(data$Speed_mph_, na.rm=TRUE)
  meann <- mean(data$Speed_mph_, na.rm=TRUE)
  
  if(type=="both"){
    data_errors <- data %>%
      mutate(cydr_SpeedError = Speed_mph_ < (meann - 2*std_dev) |
               Speed_mph_ > (meann + 2*std_dev))
  } else if (type=="low"){
    data_errors <- data %>%
      mutate(cydr_SpeedError = Speed_mph_ < (meann - 2*std_dev))
  } else if (type=="high"){
    data_errors <- data %>%
      mutate(cydr_SpeedError = Speed_mph_ > (meann + 2*std_dev))
  }
  
  if (remove)
    data_errors <- data_errors %>%
    filter(is.na(cydr_SpeedError) | !cydr_SpeedError)
  
  return(data_errors)
}

ggplot(field, aes(coords.x1, coords.x2, colour=Speed_mph_)) + geom_point()