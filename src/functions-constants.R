########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Sun Apr 22 10:57:38 2012
# Email: diegovalle at gmail.com
# Purpose: useful functions and constants
# Copyright (c) Diego Valle-Jones. All rights reserved

express.divorce.date <- as.Date("2008-10-06")


addSource <- function(plot, text = "Data Source: Vital Statistics INEGI") {
  p <- arrangeGrob(p, 
              sub = textGrob(text,
                x = 0, hjust = -0.1, vjust=0.1,
                gp = gpar(fontface = "italic", fontsize = 9,
                  col = "gray50")))
  return(p)
}


