breakToDate <- function(breakp) {
  marriage.num[bp.marriage.conf$confint[[breakp]], "date"]
}


ggplot(marriages.df, aes(date, marriages)) +
  geom_line() +
  ##scale_x_date(limits = c(as.Date("2004-01-01"), as.Date("2009-12-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2007-04-01")+ months(9)) , linetype = 2)



marriage.ts <- ts(log(marriages.df$marriages), freq = 12, start = 1993)
#png("graphs/seasonplot.png"
#fit <- auto.arima(marriage.num)
#png("graphs/forecast.png")
#plot(forecast(fit))
#dev.off()
marriage.num <- cbind(marriage.ts, lag(marriage.ts, k = -1),
                      lag(marriage.ts, k = -2),
                      lag(marriage.ts, k = -12))
marriage.num <- window(marriage.num, start = c(1994,1), end = c(2009,12))
colnames(marriage.num) <- c("y", "ylag1", "ylag2", "ylag12")

bp.marriage <- breakpoints(y ~ ylag1 + ylag12, data = marriage.num, 
                       breaks = 2, h = 12)
summary(bp.marriage)
bp.marriage.conf <- confint(bp.marriage)
bp.marriage.conf
plot(marriage.num[,"y"])
lines(bp.marriage)
lines(bp.marriage.conf)

marriage.num <- as.data.frame(marriage.num)
plot(bcp(marriage.num$y))
marriage.num[168,]
marriage.num$date <- seq(as.Date("1994-01-15"), as.Date("2009-12-15"), by = "month")


marriage.num.conf <- data.frame(start = c(breakToDate(1),breakToDate(2) ),
                       end =  c(breakToDate(5),breakToDate(6)),
                       y = 0, date = as.Date("2008-09-01"))
marriage.num.legend = data.frame(date = as.Date("2007-04-15"),
  y = 8.6, label = "legal abortion")

abortion.date <- as.Date("2007-04-27") #+ months(9)
ggplot(marriage.num, aes(date, y)) +
  geom_rect(aes(xmin = start, xmax = end,
                ymin = -Inf, ymax = Inf),
            data = marriage.num.conf,
            fill = "red", alpha = .3,
            show_guide= TRUE) +
  geom_line() +
  geom_vline(xintercept = c(as.numeric(breakToDate(4)), as.numeric(breakToDate(3))),
             linetype = 2) +
  ylim(c(7.5, 8.75)) +
  scale_x_date(limits = c(as.Date("2004-09-01"), as.Date("2009-12-01"))) +
#  geom_text(aes(date, y,
 #               label = label), data = marriage.num.legend,
  #          hjust = 1.03, vjust = 0) +
  ylab("log number of marriages") +
  xlab("marriage date") +
  opts(plot.title = theme_text(size = 12),
       title = "Monthy marriages in the Federal District (95% CI of breakpoints shown in red)") 
ggsave("graphs/breakpoint-marriage.png", dpi = 100, w = 9, h = 5)
