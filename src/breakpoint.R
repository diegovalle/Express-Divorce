

seasonggplot <- function(ts) {
  ##ts = time series with the number of divorces in mexico
  df <- data.frame(divorces = ts,
           month = factor(month.abb, levels = month.abb),
           year = floor(time(ts)))
  df$group <- ifelse(df$year == 2009, "2009", "1993-2008")
  ggplot(df, aes(month, divorces, group = year, color = group)) +
    geom_line() +
      ylab("log number of divorces") +
        opts(title = "Seasonal variation in divorces in the Federal District") +
    scale_color_manual("year", values = c(colorRampPalette(c("gray80",
                                 "black"))(2009-1993),"#E34A33")) +
    scale_x_discrete("month divorce was filed", breaks = month.abb,
                      labels = month.abb)
}


div <- ts(log(div.df$divorces), freq = 12, start = 1993)
#png("graphs/seasonplot.png")
#seasonplot(div)
#dev.off()
seasonggplot(div)
ggsave("graphs/seasonggplot.png", dpi = 100, w = 9, h = 5)
acf(div)
pacf(div)
#div <- diff(div)

fit <- auto.arima(div)
png("graphs/forecast.png")
plot(forecast(fit))
dev.off()
div <- cbind(div, lag(div, k = -1),  lag(div, k = -2), filter(div, 2),
             lag(div, k = -12))
div <- window(div, start = c(1994,1), end = c(2009,12))
colnames(div) <- c("y", "ylag1", "ylag2", "ma2", "ylag12")

bp.seat <- breakpoints(y ~ ylag1 + ylag12, data = div, 
                       breaks = 1, h = 12)
summary(bp.seat)
bp.conf <- confint(bp.seat)
bp.conf
plot(div[,"y"])
lines(bp.seat)
lines(bp.conf)
lines(as.Date("2008-09-03"))


##y <- div[,"y"]
##ylag1 <- div[,"ylag1"]
##ylag12 <- div[,"ylag12"]

## set.seed(10)
## fit.mcmc <- MCMCpoissonChange(formula = y ~ ylag1 + ylag12, data = div, m = 0,
##                   mcmc = 100, burnin = 100, verbose = 100)
## plotState(fit.mcmc, legend.control = c(1, 0.6))
## plotChangepoint(fit.mcmc)


div <- as.data.frame(div)
plot(bcp(div$y))
div$date <- seq(as.Date("1994-01-01"), as.Date("2009-12-01"), by = "month")
div.conf <- data.frame(start = div[bp.conf$confint[1], "date"],
                       end =  div[bp.conf$confint[3], "date"],
                       y = 0, date = as.Date("2008-09-01"))
div.legend = data.frame(date = as.Date("2008-10-15"),
  y = 7.1, label = "Express\nDivorce")

ggplot(div, aes(date, y)) +
  geom_rect(aes(xmin = start, xmax = end,
                ymin = -Inf, ymax = Inf),
            data = div.conf,
            fill = "red", alpha = .3,
            show_guide= TRUE) +
  geom_line() +
  geom_vline(xintercept = as.numeric(express.divorce.date),
             linetype = 2) +
  ylim(c(5.48, 7.3)) +
  geom_text(aes(date, y,
                label = label), data = div.legend,
            hjust = -0.03, vjust = 0) +
  ylab("log number of divorces") +
  xlab("date divorce was filed") +
  opts(plot.title = theme_text(size = 12),
       title = "Monthy divorces in the Federal District started rising after express divorce went into effect\n (the implementation of express divorce is marked by a dotted line, breakpoint 95% confidence interval in red)") 
ggsave("graphs/breakpoint.png", dpi = 100, w = 9, h = 5)

png("graphs/regressions-with-and-without-breakpoint.png")
fm0 <- lm(y ~ ylag1 + ylag12, data = div)
fm1 <- lm(y ~ breakfactor(bp.seat)/(ylag1 + ylag12) - 1, data = div)
plot(div[,"y"])
lines(fitted(fm0), col = 3)
lines(fitted(fm1), col = 4)
title("regressions with and without breakpoint")
dev.off()
