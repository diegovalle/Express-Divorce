
p <- ggplot(marriage.duration, aes(length, percent.divorce, group = marriage.year,
                         color = marriage.year)) +
  geom_line() +
  scale_y_continuous(label = percent)+
      xlab("years since wedding") +
  opts(title = "Proportion of marriages ending in divorce each year in all of Mexico,\n by year of marriage") +
      ylab("proportion of marriages ending in divorce") +
  scale_colour_gradient(low="gray80", high="black") +
  xlim(0,17)
direct.label(p, "last.bumpup")
ggsave("graphs/divorces-by-length.png", dpi = 100, w = 8, h = 6)

p <- ggplot(marriage.duration, aes(length, cumulative.divorce, group = marriage.year,
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
direct.label(p, "top.bumpup")
ggsave("graphs/cumulative-divorce-by-length.png", dpi = 100, w = 8, h = 6)

marriage.duration$express.divorce <- marriage.duration$divorce.year >= 2008
fit <- lme(fixed = percent.divorce ~ length + log(length) + marriage.year + express.divorce,
           random = ~ -1 | marriage.year, 
           data = subset(marriage.duration, length >= 2))
summary(fit)
plot(fit)
#marriage.duration$predict[marriage.duration$length > 0] <- predict(fit)


rep <- c()
for(i in 16:2) rep <- c(rep, 16:(i-1))
newdata <- data.frame(marriage.year = rep(1994:2008, 2:16) ,
           length = rep
)

newdata$fit <- TRUE
newdata$express.divorce <- TRUE
#newdata$length <- newdata$length^2
newdata$percent.divorce <- predict(fit, newdata)
newdata <- ddply(newdata, .(marriage.year), transform,
      percent.divorce = c(percent.divorce[1:(length(percent.divorce)-1)], NA))


newdata <- na.omit(newdata)
#with(newdata, newdata[(newdata$marriage.year %in% na.data$marriage.year &
 #                      newdata$length %in% na.data$length),])


#newdata <- rbind.fill(newdata,
 #     data.frame(marriage.year = c(1993),
  #               length = c(16),
   #              percent.divorce = c(0.0035221910)))
                        
                #newdata <- subset(newdata, length > 0)
newdata$cumulative.divorce <- NULL
#newdata$predict <- exp(newdata$predict)

marriage.duration2 <- rbind.fill(marriage.duration, newdata)
marriage.duration2 <- marriage.duration2[order(marriage.duration2$length, marriage.duration2$marriage.year),]
marriage.duration2 <- ddply(marriage.duration2, .(marriage.year), transform, 
      cumulative.divorce = cumsum(percent.divorce))

#marriage.duration2 <- merge(marriage.duration2, na.data, all = TRUE)

p <- ggplot(marriage.duration2, aes(length, percent.divorce, group = marriage.year,
                         color = marriage.year)) +
  geom_line() +
  scale_y_continuous(label = percent)+
  scale_colour_gradient(low="gray80", high="black")+
  xlab("years since wedding") +
  ylab("proportion of marriages ending in divorce")+
  opts(
       title = "Projection of proportion of marriages ending in divorce in all of Mexico,\n by year of marriage" )+
  xlim(0,17)
direct.label(p, "last.bumpup")
ggsave("graphs/projection-divorces-by-length.png", dpi = 100, w = 8, h = 6)

p <- ggplot(subset(marriage.duration2, marriage.year <= 2007),
            aes(length, cumulative.divorce, group = marriage.year,
                         color = marriage.year)) +
  geom_line(linetype = 2) +
  geom_line(data = subset(marriage.duration2, is.na(fit)),
            aes(length, cumulative.divorce, group = marriage.year,
                         color = marriage.year), linetype = 1,
            size = .7) +
  scale_y_continuous(label = percent)+
  scale_colour_gradient(low="gray80", high="black")+
  xlab("years since wedding") +
  ylab("cumulative proportion of marriages ending in divorce")+
  opts(plot.title = theme_text(size = 13),
       title = "Projection of the cumulative proportion of marriages ending in divorce in all of Mexico,\n by year of marriage" )+
  xlim(0,17)
direct.label(p, "last.bumpup")
ggsave("graphs/projection-cumulative-divorces-by-length.png", dpi = 100, w = 8, h = 6)
