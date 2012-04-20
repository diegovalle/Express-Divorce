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

names(marriage.duration) <- c("divorce.year", "marriage.year", "divorces")
marriage.duration$length <- with(marriage.duration, divorce.year - marriage.year)
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
names(div.df.state) <- c("state", "divorce.year", "divorces")
div.df.state$state <- str_replace(div.df.state$state, " de.*", "")

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

names(marriage.duration.df) <- c("divorce.year", "marriage.year", "divorces")
marriage.duration.df$length <- with(marriage.duration.df, divorce.year - marriage.year)
marriage.duration.df <- marriage.duration.df[order(marriage.duration.df$marriage.year),]
## marriage.duration.df <- subset(marriage.duration.df, divorce.year >= 1993 &
##                                marriage.year >= 1993 & divorce.year < 2010 &
##                                marriage.year < 2010)
marriage.duration.df <- subset(marriage.duration.df, divorce.year < 2010 &
                               marriage.year < 2010 & divorce.year > 1992)
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

