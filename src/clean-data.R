###################################################################3
#Load divorces that took place in the Federal District
div.df <- read.csv("data/divorces-df.csv",  skip = 6, fileEncoding = "UTF-8")
div.df <- subset(div.df, X != "Total" & X.1 != "Total" &
                 X != "No especificado" & X.1 != "No especificado")
div.df$X <- as.numeric(as.character(div.df$X))
div.df <- subset(div.df, X >= 1993 & X <= 2009)
div.df[is.na(div.df)] <- 0
div.df$divorces <-rowSums(div.df[,3:ncol(div.df)])
div.df <- div.df[,c(1,2,ncol(div.df))]

div.df$X.1 <- as.character(car::recode(div.df$X.1, "'Enero' = 'Jan';
            'Febrero' = 'Feb';
                                             'Marzo' = 'Mar';
                                             'Abril' = 'Apr';
                                             'Mayo' = 'May';
                                             'Junio' = 'Jun';
                                             'Julio' = 'Jul';
                                             'Agosto' = 'Aug';
                                             'Septiembre' = 'Sep';
                                             'Octubre' = 'Oct';
                                             'Noviembre' = 'Nov';
                                             'Diciembre' = 'Dec'"))
div.df$date <- as.Date(str_c(div.df$X, "-", div.df$X.1, "-15"), 
                          format = "%Y-%b-%d")
div.df$X <- NULL; div.df$X.1 <- NULL

##########################################################
#Clean the marriage duration data
marriage.duration <- read.csv("data/social-duration.csv", skip = 5, fileEncoding = "UTF-8")
marriage.duration <- subset(marriage.duration, X != "Total" & X.1 != "Total" &
  X != "No especificado" & X.1 != "No especificado")
marriage.duration[,1:2]<- apply(marriage.duration[,1:2], 2, function(x) as.numeric(as.character(x)))
marriage.duration[is.na(marriage.duration)] <- 0
marriage.duration$divorces <- rowSums(marriage.duration[,3:ncol(marriage.duration)])

marriage.duration$divorces[marriage.duration$X == 2009][54:70] <- (1.01 + (marriage.duration[marriage.duration$X == 2008, (ncol(marriage.duration)-1)] / marriage.duration$divorces[marriage.duration$X == 2008])[59:75]) * marriage.duration$divorces[marriage.duration$X == 2009][54:70] 

marriage.duration <- subset(marriage.duration, X >= 1993 & X.1 >= 1993 &
  X < 2010 & X.1 < 2010)

marriage.duration$divorces[marriage.duration$X == 2008] <- marriage.duration$divorces[marriage.duration$X == 2008] * 1.01
marriage.duration <- marriage.duration[,c(1,2,ncol(marriage.duration))]

names(marriage.duration) <- c("divorce.filed", "marriage.year", "divorces")
marriage.duration$marriage.length <- with(marriage.duration, divorce.filed - marriage.year)
marriage.duration <- marriage.duration[order(marriage.duration$marriage.year),]

#Clean marriage number data
mar<- read.csv("data/marriages.csv", skip = 5, fileEncoding = "UTF-8")
mar <- data.frame(marriages = unlist(mar[1,3:ncol(mar)]),
                  marriage.year = 1993:2009)

#Merge with the marriage duration data
marriage.duration <- merge(marriage.duration, mar)
marriage.duration$percent.divorce <- marriage.duration$divorces / marriage.duration$marriages
marriage.duration <- ddply(marriage.duration, .(marriage.year), transform, 
      cumulative.divorce = cumsum(percent.divorce))

######################################################
#Load the federal district divorce data by state of marriage of the persons who solicited a divorce
div.df.state <- read.csv("data/divorce-registered-df.csv", skip = 7, fileEncoding = "UTF-8")
div.df.state$X <- NULL
div.df.state <- subset(div.df.state, X.1 != "Total" & X.2 != "Total" &
  X.1 != "No especificado" & X.2 != "No especificado")
div.df.state[,2]<- as.numeric(as.character(div.df.state[,2]))
div.df.state[is.na(div.df.state)] <- 0
div.df.state$divorces <- rowSums(div.df.state[,3:ncol(div.df.state)])



div.df.state <- subset(div.df.state, X.2 >= 1993 &
  X.2 < 2010)

div.df.state$divorces[div.df.state$X == 2008] <- div.df.state$divorces[div.df.state$X == 2008] * 1.01
div.df.state <- div.df.state[,c(1,2,ncol(div.df.state))]
names(div.df.state) <- c("marriage.state", "divorce.filed", "divorces")
div.df.state$marriage.state <- str_replace(div.df.state$marriage.state, " de.*", "")

###################################################
#Marriage duration in the Federal district
marriage.duration.df <- read.csv("data/social-duration-df.csv",
                                 skip = 6, fileEncoding = "UTF-8")

marriage.duration.df <- subset(marriage.duration.df, X != "Total" & X.1 != "Total" &
  X != "No especificado" & X.1 != "No especificado")
marriage.duration.df[,1:2]<- apply(marriage.duration.df[,1:2], 2, function(x) as.numeric(as.character(x)))
marriage.duration.df[is.na(marriage.duration.df)] <- 0
marriage.duration.df$divorces <- rowSums(marriage.duration.df[,3:ncol(marriage.duration.df)])

marriage.duration.df <- marriage.duration.df[,c(1,2,ncol(marriage.duration.df))]

names(marriage.duration.df) <- c("divorce.filed", "marriage.year", "divorces")
marriage.duration.df$marriage.length <- with(marriage.duration.df, divorce.filed - marriage.year)
marriage.duration.df <- marriage.duration.df[order(marriage.duration.df$marriage.year),]
## marriage.duration.df <- subset(marriage.duration.df, divorce.filed >= 1993 &
##                                marriage.year >= 1993 & divorce.filed < 2010 &
##                                marriage.year < 2010)
marriage.duration.df <- subset(marriage.duration.df, divorce.filed < 2010 &
                               marriage.year < 2010 & divorce.filed > 1992)
#head(marriage.duration.df)

#######################################################
#Marriages in the Federal District
marriages.df <- read.csv("data/marriages-df.csv", skip = 6, fileEncoding = "UTF-8")
marriages.df <- subset(marriages.df, X != "Total" & X != "FUENTE: INEGI. Estadísticas de nupcialidad.")
marriages.df$X <- month.abb
marriages.df <- melt(marriages.df, id = "X")
marriages.df$date <- with(marriages.df, as.Date((str_c(X, variable, "01", sep = "-")),
                                                "%b-X%Y-%d")
                          )
marriages.df$date <- as.Date(marriages.df$date)
marriages.df <- marriages.df[,3:4]
names(marriages.df) <- c("marriages", "date")

#####################################################################
#Marriage duration by state
marriage.duration.state <- read.csv("data/social-duration-by-state.csv.bz2",
                                    skip = 3, fileEncoding = "windows-1252")

marriage.duration.state$X.2 <- NULL
marriage.duration.state <- subset(marriage.duration.state, X != "Total" &
                                  X.1 != "Total" & X.3 != "Total" &
  X != "No especificado" & X.1 != "No especificado" & X.3 != "No especificado")
marriage.duration.state[,1:2]<- apply(marriage.duration.state[,1:2], 2, function(x) as.numeric(as.character(x)))
marriage.duration.state[,4:ncol(marriage.duration.state)]<- apply(marriage.duration.state[,4:ncol(marriage.duration.state)], 2, function(x) as.numeric(str_replace(x, ",", "")))

marriage.duration.state[is.na(marriage.duration.state)] <- 0
marriage.duration.state$divorces <- rowSums(marriage.duration.state[,4:ncol(marriage.duration.state)])


marriage.duration.state <- subset(marriage.duration.state, X >= 1993 & X.1 >= 1993 &
  X < 2010 & X.1 < 2010)

#marriage.duration.state$divorces[marriage.duration.state$X == 2008] <- marriage.duration.state$divorces[marriage.duration.state$X == 2008] * 1.01
marriage.duration.state <- marriage.duration.state[,c(1:3,ncol(marriage.duration.state))]

names(marriage.duration.state) <- c("divorce.filed", "marriage.year", "marriage.state", "divorces")
marriage.duration.state$marriage.length <- with(marriage.duration.state, divorce.filed - marriage.year)

#Clean marriage number data
mar<- read.csv("data/marriages.csv", skip = 5, fileEncoding = "UTF-8")
mar$X <- NULL
mar <- subset(mar, X.1 != "Total" & X.1 != "")
mar <- melt(mar, id = "X.1")
mar$variable <- as.numeric(str_replace(mar$variable, "X", ""))
names(mar) <- c("marriage.state", "marriage.year","marriages")

##Merge with the marriage duration data
marriage.duration.state <- merge(marriage.duration.state, mar, all.x = TRUE)
marriage.duration.state$percent.divorce <- with(marriage.duration.state, divorces / marriages)
marriage.duration.state <- marriage.duration.state[order(marriage.duration.state$marriage.year, marriage.duration.state$marriage.length),]

marriage.duration.state <- ddply(marriage.duration.state, .(marriage.year, marriage.state), transform, 
      cumulative.divorce = cumsum(percent.divorce))
marriage.duration.state$marriage.state <- str_replace(marriage.duration.state$marriage.state, " de.*", "")


######################################################################
#Marriage duration in the Federal district of marriages that took place in the Federal District
marriage.duration.df.df <- read.csv("data/social-duration-df-marriage-df.csv",
                                 skip = 6, fileEncoding = "UTF-8")

marriage.duration.df.df <- subset(marriage.duration.df.df, X != "Total" & X.1 != "Total" &
  X != "No especificado" & X.1 != "No especificado")
marriage.duration.df.df[,1:2]<- apply(marriage.duration.df.df[,1:2], 2, function(x) as.numeric(as.character(x)))
marriage.duration.df.df[is.na(marriage.duration.df.df)] <- 0
marriage.duration.df.df$divorces <- rowSums(marriage.duration.df.df[,3:ncol(marriage.duration.df.df)])

marriage.duration.df.df <- marriage.duration.df.df[,c(1,2,ncol(marriage.duration.df.df))]

names(marriage.duration.df.df) <- c("divorce.filed", "marriage.year", "divorces")
marriage.duration.df.df$marriage.length <- with(marriage.duration.df.df, divorce.filed - marriage.year)
marriage.duration.df.df <- marriage.duration.df.df[order(marriage.duration.df.df$marriage.year),]
## marriage.duration.df.df <- subset(marriage.duration.df.df, divorce.filed >= 1993 &
##                                marriage.year >= 1993 & divorce.filed < 2010 &
##                                marriage.year < 2010)
marriage.duration.df.df <- subset(marriage.duration.df.df, divorce.filed < 2010 &
                               marriage.year < 2010 & divorce.filed > 1992)


#############################################################################333
##Marriage duration in the federal district by state
marriage.duration.df.state <- read.csv("data/social-duration-df-by-state.csv.bz2",
                                    skip = 4, fileEncoding = "windows-1252")

marriage.duration.df.state$X.2 <- NULL
marriage.duration.df.state <- subset(marriage.duration.df.state, X != "Total" &
                                  X.1 != "Total" & X.3 != "Total" &
  X != "No especificado" & X.1 != "No especificado" & X.3 != "No especificado")
marriage.duration.df.state[,1:2]<- apply(marriage.duration.df.state[,1:2], 2, function(x) as.numeric(as.character(x)))
marriage.duration.df.state[,4:ncol(marriage.duration.df.state)]<- apply(marriage.duration.df.state[,4:ncol(marriage.duration.df.state)], 2, function(x) as.numeric(str_replace(x, ",", "")))

marriage.duration.df.state[is.na(marriage.duration.df.state)] <- 0
marriage.duration.df.state$divorces <- rowSums(marriage.duration.df.state[,4:ncol(marriage.duration.df.state)])


marriage.duration.df.state <- subset(marriage.duration.df.state, X >= 1993 & X < 2010)

#marriage.duration.df.state$divorces[marriage.duration.df.state$X == 2008] <- marriage.duration.df.state$divorces[marriage.duration.df.state$X == 2008] * 1.01
marriage.duration.df.state <- marriage.duration.df.state[,c(1:3,ncol(marriage.duration.df.state))]

names(marriage.duration.df.state) <- c("divorce.filed", "marriage.year", "marriage.state", "divorces")
marriage.duration.df.state$marriage.length <- with(marriage.duration.df.state, divorce.filed - marriage.year)

marriage.duration.df.state$group <- ifelse(marriage.duration.df.state$marriage.state == "Distrito Federal", "DF", "Elsewhere")
marriage.duration.dfe.state <- ddply(marriage.duration.df.state, .(divorce.filed, marriage.year, group),
      summarise,
      divorces = sum(divorces),
      marriage.length = marriage.length[1])
