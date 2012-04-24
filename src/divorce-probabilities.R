########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Thu Apr 19 21:36:38 2012
# Email: diegovalle at gmail.com
# Purpose: Plots of the proportion of marriages that end in divorce 
# Copyright (c) Diego Valle-Jones. All rights reserved

p <- ggplot(marriage.duration, aes(marriage.length, percent.divorce, group = marriage.year,
                         color = marriage.year)) +
  geom_line() +
  scale_y_continuous(label = percent)+
      xlab("years since wedding") +
  opts(title = "Proportion of marriages ending in divorce each year in all of Mexico,\n by year of marriage") +
      ylab("proportion of marriages ending in divorce") +
  scale_colour_gradient(low="gray80", high="black") +
  xlim(0,17)
p <- direct.label(p, "last.bumpup")
p <- addSource(p, "Data Source: Vital Statistics INEGI (2009 and 2008 data has been adjusted for undercounting)")
ggsave("graphs/divorces-by-length.png", plot = p, dpi = 100, w = 8, h = 6)

p <- ggplot(marriage.duration, aes(marriage.length, cumulative.divorce, group = marriage.year,
                         color = marriage.year)) +
      geom_line() +
      scale_y_continuous(label = percent) +
      xlab("years since wedding") +
      ylab("cumulative proportion of marriages ending in divorce") +
      scale_colour_gradient(low="gray80", high="black")+
      theme_bw() +
      opts(legend.position="none",
           title = "Cumulative proportion of marriages ending in divorce in all of Mexico,\n by year of marriage" )+
  xlim(0,17)
p <- direct.label(p, "top.bumpup")
p <- addSource(p, "Data Source: Vital Statistics INEGI (2009 and 2008 data has been adjusted for undercounting)")
ggsave("graphs/cumulative-divorce-by-length.png", plot = p, dpi = 100, w = 8, h = 6)

#Fit a multilevel model to the percentage of marriages that end in divorce
marriage.duration$express.divorce <- marriage.duration$divorce.filed >= 2008
fit <- lme(fixed = percent.divorce ~ marriage.length + log(marriage.length) + marriage.year + express.divorce,
           random = ~ -1 | marriage.year, 
           data = subset(marriage.duration, marriage.length >= 2))
summary(fit)
plot(fit)
#marriage.duration$predict[marriage.duration$marriage.length > 0] <- predict(fit)

#Create a data frame with the years we want to preditc
rep <- c()
for(i in 16:2) rep <- c(rep, 16:(i-1))
newdata <- data.frame(marriage.year = rep(1994:2008, 2:16) ,
           marriage.length = rep
)
newdata$fit <- TRUE
newdata$express.divorce <- TRUE
#newdata$marriage.length <- newdata$marriage.length^2
newdata$percent.divorce <- predict(fit, newdata)
newdata <- ddply(newdata, .(marriage.year), transform,
      percent.divorce = c(percent.divorce[1:(length(percent.divorce)-1)], NA))
newdata <- na.omit(newdata)
newdata$cumulative.divorce <- NULL
marriage.duration2 <- rbind.fill(marriage.duration, newdata)
marriage.duration2 <- marriage.duration2[order(marriage.duration2$marriage.length, marriage.duration2$marriage.year),]
marriage.duration2 <- ddply(marriage.duration2, .(marriage.year), transform, 
      cumulative.divorce = cumsum(percent.divorce))

#Charts with the predictions
p <- ggplot(marriage.duration2, aes(marriage.length, percent.divorce, group = marriage.year,
                         color = marriage.year)) +
  geom_line() +
  scale_y_continuous(label = percent)+
  scale_colour_gradient(low="gray80", high="black")+
  xlab("years since wedding") +
  ylab("proportion of marriages ending in divorce")+
  opts(
       title = "Projection of proportion of marriages ending in divorce in all of Mexico,\n by year of marriage" )+
  xlim(0,17)
p <- direct.label(p, "last.bumpup")
p <- addSource(p, "Data Source: Vital Statistics INEGI (2009 and 2008 data has been adjusted for undercounting)")
ggsave("graphs/projection-divorces-by-length.png", plot = p, dpi = 100, w = 8, h = 6)

p <- ggplot(subset(marriage.duration2, marriage.year <= 2007),
            aes(marriage.length, cumulative.divorce, group = marriage.year,
                         color = marriage.year)) +
  geom_line(linetype = 2) +
  geom_line(data = subset(marriage.duration2, is.na(fit)),
            aes(marriage.length, cumulative.divorce, group = marriage.year,
                         color = marriage.year), linetype = 1,
            size = .7) +
  scale_y_continuous(label = percent)+
  scale_colour_gradient(low="gray80", high="black")+
  xlab("years since wedding") +
  ylab("cumulative proportion of marriages ending in divorce")+
  opts(plot.title = theme_text(size = 13),
       title = "Projection of the cumulative proportion of marriages ending in divorce in all of Mexico,\n by year of marriage" )+
  xlim(0,17)
p <- direct.label(p, "last.bumpup")
p <- addSource(p, "Data Source: Vital Statistics INEGI (2009 and 2008 data have been adjusted for undercounting)")
ggsave("graphs/projection-cumulative-divorces-by-length.png", plot = p,
       dpi = 100, w = 8, h = 6)
