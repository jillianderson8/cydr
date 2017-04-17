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

#' One line description
#' @description description
#' @param param1 a dataframe
#' @param param2 a boolean. Defaults to FALSE.
#' @return Value
#' @examples
#' id_pass_end(field1)
#' id_pass_end(field1, remove=TRUE)
summarize_cydrErrors <- function(data){
  return(data)
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