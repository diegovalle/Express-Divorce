########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Thu Apr 19 21:38:25 2012
# Email: diegovalle at gmail.com
# Purpose: People from outside the federal district and lengthy marriages are the ones getting divorced! 
# Copyright (c) Diego Valle-Jones. All rights reserved


#data frame with the difference in divorces from 2009 to 2008
per.change <- ddply(div.df.state, .(marriage.state),
                    summarise,
                    ratio = divorces[length(divorces)] - divorces[length(divorces)-1])
per.change$change <- ifelse(per.change$ratio > 0, "increase", "decrease")
per.change$change[per.change$ratio == 0] <- "no change"
per.change$marriage.state <- with(per.change, reorder(marriage.state, ratio))
p <- ggplot(per.change,
       aes(ratio, marriage.state, color = change)) +
  geom_vline(xintercept = 1,linetype = 2, color = "gray40") +
  geom_point(size = 3) +
  opts(title = "Change in number of divorces filed in the Federal District between 2009 and 2008") +
  xlab("change in number of divorces") +
  ylab("state where marriage took place") +
  scale_color_manual("change\nin divorces\n2009-2008", values = c("#67A9CF",
                                              "#EF8A62", "#FEE090"),
                     breaks = c("increase", "no change", "decrease"))
p <- addSource(p)
ggsave("graphs/ratio-df.png", plot = p, dpi = 100, w = 8, h = 6)


###############
##Bug Alert!
##############
##Repeat the data for the Federal district because otherwise direct.label throws an error
div.df.state2 <- rbind(subset(div.df.state, marriage.state == "Distrito Federal"), div.df.state)
div.df.state2$FederalDistrict <- ifelse(div.df.state2$marriage.state %in% c("Distrito Federal"),
                                 "Federal District", "Elsewhere")
p <- ggplot(div.df.state2,
       aes(divorce.filed, divorces,
           group = marriage.state, color = marriage.state,
           label = marriage.state)) +
  geom_line(show_guide= FALSE)  +
  scale_y_log10() +
  xlim(2005,2011.4) +
  facet_wrap(~ FederalDistrict, scale = "free_y") +
  opts(title = "Divorce trends in the Federal District, by state where marriage took place") +
  xlab("year divorce was filed") +
  ylab("number of divorces")
p <- direct.label(p, "last.bumpup")
p <- addSource(p)
ggsave("graphs/divorce-trends-df-vs-elsewhere.png", plot = p, dpi = 100, w = 9, h = 6,
        type = "cairo")

## ggplot(div.df.state,
##        aes(divorce.filed, divorces,
##            group = marriage.state, color = marriage.state,
##            label = marriage.state)) +
##   geom_line(show_guide= FALSE)  +
##   geom_text(data = subset(div.df.state, divorce.filed == 2009),
##             aes(divorce.filed, divorces), show_guide= FALSE,
##             hjust = 0) +
##   scale_y_log10() +
##   xlim(2005,2011) +
##   facet_wrap(~ FederalDistrict, scale = "free_y")
## #ggsave("graphs/ratio-df.png", dpi = 100, w = 9, h = 7)


