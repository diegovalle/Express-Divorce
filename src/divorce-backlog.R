

marriage.duration.df$group <- cut(marriage.duration.df$length,
                                  c(0,2,5,10,15, Inf),
                                  right = FALSE,
                                  labels = c("0-1 years","2-4 years",
                                    "5-9 years","10-14 years","15 or more years"))

df.length <- ddply(marriage.duration.df, .(divorce.year, group), summarise,
      divorces = sum(divorces))

div.table <- xtabs(divorces ~ divorce.year + group ,
                 data = subset(df.length, divorce.year %in% 2007:2009))
print(xtable(div.table, digits = 0 ), type = "html")

                                         
df.length <- ddply(df.length, .(group), transform,
                   divorces = Delt(divorces, type= 'arithmetic'))

p <- ggplot(df.length, aes(divorce.year, divorces,
                           group = group, color = group)) +
  geom_line() +
  xlim(2001,2011) +
  opts(title = "Percentage change in number of divorces, by length of marriage") +
  xlab("year divorce was filed") +
  ylab("percentage change in number of divorces") +
  scale_y_continuous(label = percent)
direct.label(p, "last.bumpup")
ggsave("graphs/divorce-backlog.png", dpi = 100, w = 8, h = 5)
