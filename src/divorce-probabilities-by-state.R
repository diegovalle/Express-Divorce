##marriage.duration.state2 <- subset(marriage.duration.state,
  ##                                marriage.year %in% c(1994, 1997, 2000))
marriage.duration.state2 <- ddply(marriage.duration.state, .(marriage.state),
                                  transform, order = max(percent.divorce))
marriage.duration.state2$state <- with(marriage.duration.state2, reorder(marriage.state,
                                                                       -percent.divorce))
p <- ggplot(marriage.duration.state2, aes(marriage.length,
                                         percent.divorce,
                                         group = marriage.year,
                                         color = marriage.year)) +
  geom_line() +
  scale_y_continuous(label = percent, breaks = c(0,.005, .015)) +
      xlab("years since wedding") +
  opts(title = "Proportion of marriages ending in divorce each year in all of Mexico,\n by year of marriage and state where marriage took place") +
      ylab("proportion of marriages ending in divorce") +
  scale_colour_gradient("year of\nmarriage", low="gray80", high="black") +
  xlim(0,17) +
  facet_wrap(~state)
p <- addSource(p)
ggsave("graphs/divorces-by-length-state.png", plot = p, dpi = 100, w = 9, h = 6)


marriage.duration.state2 <- subset(marriage.duration.state2,
                                   state %in% c("Hidalgo",
                                                "Distrito Federal",
                                                "Querétaro",
                                                "Morelos"))
marriage.duration.state2 <- ddply(marriage.duration.state2, .(marriage.state),
                                  transform, order = max(percent.divorce))
marriage.duration.state2$state <- with(marriage.duration.state2, reorder(marriage.state,
                                                                       -percent.divorce))
p <- ggplot(marriage.duration.state2, aes(marriage.length,
                                         percent.divorce,
                                         group = marriage.year,
                                         color = marriage.year)) +
  geom_line() +
  scale_y_continuous(label = percent)+
      xlab("years since wedding") +
  opts(title = "Proportion of marriages ending in divorce each year near the Federal District,\n by year of marriage and state where marriage took place (1993-2007)") +
      ylab("proportion of marriages ending in divorce") +
  scale_colour_gradient("year of\nmarriage", low="gray80", high="black") +
  xlim(0,17) +
  facet_wrap(~state)
p <- addSource(p)
ggsave("graphs/divorces-by-length-state-df-qto-hgo-mor.png", plot = p,
       dpi = 100, w = 8, h = 6)


##marriage.duration.state2 <- subset(marriage.duration.state,
##                                  marriage.year %in% c(1993, 1997, 2000))
marriage.duration.state2 <- ddply(marriage.duration.state, .(marriage.state),
                                  transform, order = max(cumulative.divorce))

marriage.duration.state2$state <- with(marriage.duration.state2, reorder(marriage.state,
                                                                       -order))
p <- ggplot(marriage.duration.state2, aes(marriage.length, cumulative.divorce, group = marriage.year,
                         color = marriage.year)) +
      geom_line() +
      scale_y_continuous(label = percent) +
      xlab("years since wedding") +
      ylab("cumulative proportion of marriages ending in divorce") +
      scale_colour_gradient("year of\nmarriage", low="gray80", high="black")+
      theme_bw() +
      opts(
           title = "Cumulative proportion of marriages ending in divorce in all of Mexico,\n by year of marriage and state where marriage took place" )+
  xlim(0,17)+
  facet_wrap(~state)
p <- addSource(p)
ggsave("graphs/cumulative-divorce-by-length-state.png", plot = p,
       dpi = 100, w = 9, h = 6)


mx.map <- readOGR("maps/Mexico.shp", "Mexico")
mx.map <- fortify(mx.map, id = "NAME")

p <- ggplot(subset(marriage.duration.state, marriage.year == 1993 &
              marriage.length == 15), aes(map_id = marriage.state)) +
  geom_map(aes(fill = cumulative.divorce), map = mx.map) +
  scale_fill_gradient("Marriages\nending\nin divorce",
                      label = percent, low = "#FFF7EC", high = "#7F0000") +
  expand_limits(x = mx.map$long, y = mx.map$lat) +
  guides(fill = guide_colorbar(colours = topo.colors(10))) +
  opts(axis.line=theme_blank(),axis.text.x=theme_blank(),
       axis.text.y=theme_blank(),axis.ticks=theme_blank(),
       axis.title.x=theme_blank(), legend.position="right",
       axis.title.y=theme_blank(),
       panel.background=theme_blank(),panel.grid.major=theme_blank(),
       panel.grid.minor=theme_blank(),plot.background=theme_blank(),
       title = "Percentage of 1993 marriages ending in divorce after 15 years,\n by state where marriage took place")
p <- addSource(p)
ggsave("graphs/cumulative-divorce-map.png", plot = p, dpi = 100, w = 9, h = 6)
