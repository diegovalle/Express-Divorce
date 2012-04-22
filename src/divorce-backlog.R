########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Thu Apr 19 21:40:54 2012
# Email: diegovalle at gmail.com
# Purpose: Lengthy marriages are ending at a higher rate than newer ones
# Copyright (c) Diego Valle-Jones. All rights reserved


#Group marriages by length
marriage.duration.df$group <- cut(marriage.duration.df$marriage.length,
                                  c(0,2,5,10,15, Inf),
                                  right = FALSE,
                                  labels = c("0-1 years","2-4 years",
                                    "5-9 years","10-14 years","15 or more years"))

df.length <- ddply(marriage.duration.df, .(divorce.filed, group), summarise,
      divorces = sum(divorces))

#Print an html with the number of divorces by length of marriage for the last few years
div.table <- xtabs(divorces ~ divorce.filed + group ,
                 data = subset(df.length, divorce.filed %in% 2007:2009))
print(xtable(div.table, digits = 0 ), type = "html")

#The percentage change in divorces and plot  
df.length <- ddply(df.length, .(group), transform,
                   divorces = Delt(divorces, type= 'arithmetic'))

p <- ggplot(df.length, aes(divorce.filed, divorces,
                           group = group, color = group)) +
  geom_line() +
  xlim(2001,2011) +
  opts(title = "Percentage change in number of divorces filed in the Federal District,\n by length of marriage") +
  xlab("year divorce was filed") +
  ylab("annual change in number of divorces") +
  scale_y_continuous(label = percent)
p <- direct.label(p, "last.bumpup")
p <- addSource(p)
ggsave("graphs/divorce-backlog.png", plot = p, dpi = 100, w = 8, h = 5)



##############################################333
##Divorces that took place in the Federal District of Marriages that took place in the Federal District


marriage.duration.df.df$group <- cut(marriage.duration.df.df$marriage.length,
                                  c(0,2,5,10,15, Inf),
                                  right = FALSE,
                                  labels = c("0-1 years","2-4 years",
                                    "5-9 years","10-14 years","15 or more years"))

df.df.length <- ddply(marriage.duration.df.df, .(divorce.filed, group), summarise,
      divorces = sum(divorces))
div.table <- xtabs(divorces ~ divorce.filed + group ,
                 data = subset(df.df.length, divorce.filed %in% 2007:2009))
print(xtable(div.table, digits = 0 ), type = "html")
df.df.length <- ddply(df.df.length, .(group), transform,
      divorces = Delt(divorces, type= 'arithmetic'))
p <- ggplot(df.df.length,
       aes(divorce.filed, divorces, group = group, color = group)) +
  geom_line()+
  opts(title = "Percentage change in number of divorces filed in the Federal District,\n of marriages that took place in the Federal District, by length of marriage") +
  xlab("year divorce was filed") +
  ylab("annual change in number of divorces") +
  scale_y_continuous(label = percent) +
  xlim(2001, 2010.5) 
p <- direct.label(p, "last.bumpup")
p <- addSource(p)
ggsave("graphs/divorce-backlog-in-df.png", plot = p, dpi = 100, w = 8, h = 5)




#####################################################################
##All states excludig the Federal District

marriage.duration.dfe.state2 <- subset(marriage.duration.dfe.state,
                                      group == "Elsewhere")
marriage.duration.dfe.state2$group <- cut(marriage.duration.dfe.state2$marriage.length,
                                         c(0,2,5,10,15, Inf),
                                         right = FALSE,
                                         labels = c("0-1 years","2-4 years",
                                           "5-9 years","10-14 years","15 or more years"))

df.dfe.length <- ddply(marriage.duration.dfe.state2, .(divorce.filed, group), summarise,
      divorces = sum(divorces))
div.table <- xtabs(divorces ~ divorce.filed + group ,
                 data = subset(df.dfe.length, divorce.filed %in% 2007:2009))
print(xtable(div.table, digits = 0 ), type = "html")
df.dfe.length <- ddply(df.dfe.length, .(group), transform,
      divorces = Delt(divorces, type= 'arithmetic'))
p <- ggplot(df.dfe.length,
       aes(divorce.filed, divorces, group = group, color = group)) +
  geom_line()+
  opts(title = "Percentage change in number of divorces filed in the Federal District,\n of marriages that took place outside the Federal District, by length of marriage") +
  xlab("year divorce was filed") +
  ylab("annual change in number of divorces") +
  scale_y_continuous(label = percent) +
  xlim(2001, 2010.5) 
p <- direct.label(p, "last.bumpup")
p <- addSource(p)
ggsave("graphs/divorce-backlog-outside-df.png", plot = p, dpi = 100, w = 8, h = 5)
