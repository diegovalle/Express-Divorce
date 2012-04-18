div.df <- read.csv("divorce-registered-df.csv", skip = 7)
div.df$X <- NULL
div.df <- subset(div.df, X.1 != "Total" & X.2 != "Total" &
  X.1 != "No especificado" & X.2 != "No especificado")
div.df[,2]<- as.numeric(as.character(div.df[,2]))
div.df[is.na(div.df)] <- 0
div.df$divorces <- rowSums(div.df[,3:ncol(div.df)])



div.df <- subset(div.df, X.2 >= 1993 &
  X.2 < 2010)

div.df$divorces[div.df$X == 2008] <- div.df$divorces[div.df$X == 2008] * 1.01
div.df <- div.df[,c(1,2,ncol(div.df))]

names(div.df) <- c("state", "divorce.year", "divorces")

per.change <- ddply(div.df, .(state),
                    summarise,
                    ratio = divorces[length(divorces)] / divorces[length(divorces)-1])
per.change$change <- ifelse(per.change$ratio >= 1, "increase", "decrease") 
per.change$state <- with(per.change, reorder(state, ratio))
ggplot(per.change,
       aes(ratio, state, color = change)) +
  geom_vline(xintercept = 1,linetype = 2, color = "gray70") +
  geom_point() +
  opts(title = "Ratio of 2009 divorces to 2008 divorces that occured in the Federal District")



div.df <- rbind(subset(div.df, state == "Distrito Federal"), div.df)
div.df$FederalDistrict <- ifelse(div.df$state %in% c("Distrito Federal"),
                                 "Federal District", "Elsewhere")
p <- ggplot(div.df,
       aes(divorce.year, divorces,
           group = state, color = state,
           label = state)) +
  geom_line(show_guide= FALSE)  +
  scale_y_log10() +
  xlim(2005,2011) +
  facet_wrap(~ FederalDistrict, scale = "free_y") +
  opts(title = "Divorce Trends in Mexico") +
  xlab("year of divorce") +
  ylab("number of divorces")
direct.label(p, "last.bumpup")

ggplot(div.df,
       aes(divorce.year, divorces,
           group = state, color = state,
           label = state)) +
  geom_line(show_guide= FALSE)  +
  geom_text(data = subset(div.df, divorce.year == 2009),
            aes(divorce.year, divorces), show_guide= FALSE,
            hjust = 0) +
  scale_y_log10() +
  xlim(2005,2011) +
  facet_wrap(~ FederalDistrict, scale = "free_y")


head(div.df)
