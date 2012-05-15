########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Thu Apr 19 21:29:16 2012
# Email: diegovalle at gmail.com
# Purpose: Breakpoint and Chart of Divorces in the Federal District
# Copyright (c) Diego Valle-Jones. All rights reserved


seasonggplot <- function(ts) {
  ##ts = time series with the number of divorces in mexico
  df <- data.frame(divorces = ts,
           month = factor(month.abb, levels = month.abb),
           year = floor(time(ts)))
  df$group <- ifelse(df$year == 2009, "2009", "1993-2008")
  ggplot(df, aes(month, exp(divorces), group = year, color = factor(year))) +
    geom_line() +
      ylab("number of divorces") +
        opts(title = "Seasonal variation in divorces in the Federal District") +
    scale_color_manual("year", values = c(colorRampPalette(c("gray80",
                                 "black"))(2009-1993),"#E34A33"),
                       breaks = c(19----93,2009), labels = c("1993-2008", "2009")) +
    scale_x_discrete("month divorce was filed", breaks = month.abb,
                      labels = month.abb)
}


#Time series object of divorces
div <- ts(log(div.df$divorces), freq = 12, start = 1993)
#png("graphs/seasonplot.png")
#seasonplot(div)
#dev.off()
p <- seasonggplot(div)
p <- addSource(p)
ggsave("graphs/seasonggplot.png", plot = p, dpi = 100, w = 9, h = 5)
acf(div)
pacf(div)
#div <- diff(div)

fit.bfast <- bfast(div, h= .07, season = "dummy", max.iter = 1)
png("graphs/bfast-break.png")
plot(fit.bfast)
title("breakpoints estimated with bfast")
dev.off()

fit <- auto.arima(div)
png("graphs/forecast.png")
plot(forecast(fit))
dev.off()
#create an object with lags
div <- cbind(div, lag(div, k = -1),  lag(div, k = -2),
             lag(div, k = -12))
div <- window(div, start = c(1994,1), end = c(2009,12))
colnames(div) <- c("y", "ylag1", "ylag2", "ylag12")

#The breakpoint from strucchange
bp.divorce <- breakpoints(y ~ ylag1 + ylag12, data = div, 
                       breaks = 1, h = 12)
summary(bp.divorce)
bp.conf <- confint(bp.divorce)
bp.conf
#A quick plot of the breakpoint
plot(div[,"y"])
lines(bp.divorce)
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

#Objects to add nice lines  to the ggplot
div <- as.data.frame(div)
plot(bcp(div$y))
div$date <- seq(as.Date("1994-01-01"), as.Date("2009-12-01"), by = "month")
##This if for creating a bar that represents the confidence interval around
##the breakpoint, but since the breakpoint coincides with express divorce
##I decided to leave it out. I mean, what else could it be?
div.conf <- data.frame(start = div[bp.conf$confint[1], "date"],
                       end =  div[bp.conf$confint[3], "date"],
                       y = 0, date = as.Date("2008-09-01"))
div.legend = data.frame(date = as.Date("2008-10-15"),
  y = 7.1, label = "Express\nDivorce")

p <- ggplot(div, aes(date, exp(y))) +
  ## geom_rect(aes(xmin = start, xmax = end,
  ##               ymin = -Inf, ymax = Inf),
  ##           data = div.conf,
  ##           fill = "red", alpha = .3,
  ##          show_guide= TRUE) +
  geom_line() +
  geom_vline(xintercept = as.numeric(express.divorce.date),
             linetype = 2) +
  ##scale_x_date(limits = c(as.Date("2005-01-01"), as.Date("2009-12-31"))) +
  geom_text(aes(date, y,
                label = label), data = div.legend,
            hjust = -0.03, vjust = 0) +
  ylab("number of divorces") +
  xlab("date divorce was filed") +
  opts(plot.title = theme_text(size = 12),
       title = "Monthy divorces in the Federal District started rising after the express divorce law went into effect")
p <- addSource(p)
ggsave("graphs/breakpoint.png", plot = p, dpi = 100, w = 9, h = 5)


#Check that the breakpoint model makes sense
png("graphs/regressions-with-and-without-breakpoint.png")
fm0 <- lm(y ~ ylag1 + ylag12, data = div)
fm1 <- lm(y ~ breakfactor(bp.divorce)/(ylag1 + ylag12) - 1, data = div)
plot(div[,"y"])
lines(fitted(fm0), col = 3)
lines(fitted(fm1), col = 4)
title("regressions with and without breakpoint")
dev.off()
