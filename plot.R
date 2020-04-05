

myPalette <- colorRampPalette(brewer.pal(12, "Paired"))



############
### world
############

title.prediction <-
  paste(
    format(max(daily.cumulative.death$date), format = "%Y %B %d"),
    ". The peak of the epidemic = triangle. Confinement date = circle with an X. ",
    "data from https://github.com/CSSEGISandData/COVID-19."
  )

title.prediction.norm <-
  paste(
    format(max(daily.cumulative.death$date), format = "%Y %B %d"),
    "Normalized data by population size of the country. Time 0 is ~2 deaths for 1 million.",
    "data from https://github.com/CSSEGISandData/COVID-19."
  )

###################################################################################################
p.prediction <- ggplot(death.prediction, aes(date, death)) +
  geom_point() +
  geom_line() +
  geom_line(data = death.prediction,
            aes(x = date, y = prediction),
            linetype = "dashed") +
  scale_color_manual(values = myPalette(length(countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(countries.of.interest))) +
  scale_x_date(
    date_breaks = "7 days",
    date_minor_breaks = "1 day",
    limits = c(min(death.prediction$date), max.date.pred)
  ) +
  scale_y_continuous(n.breaks = 15,
                     limits = c(0, y.lim.max)) +
  geom_ribbon(
    data = death.prediction,
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.3
  ) +
  geom_point(
    data = pic.value.all,
    aes(x = date, y = peak.epidemic),
    shape = 24,
    size = 4,
    color = "darkred",
    show.legend = FALSE
  ) +
  geom_point(
    data = confinement.date.top,
    aes(x = date, y = death),
    shape = 13,
    size = 4,
    show.legend = FALSE
  ) +
  labs(y = "#deaths") +
  aes(color = country, fill = country) +
  ggtitle(title.prediction)

###################################################################################################
p.prediction.norm <-
  ggplot(death.prediction, aes(time.norm, death.norm)) +
  geom_point() +
  geom_line() +
  geom_line(
    data = death.prediction,
    aes(x = time.norm, y = prediction.norm),
    linetype = "dashed"
  ) +
  scale_color_manual(values = myPalette(length(countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(countries.of.interest))) +
  labs(y = "#deaths (per millions inhabitants)") +
  scale_y_continuous(n.breaks = 15,
                     limits = c(0, 350)) +
  geom_point(
    data = confinement.date.top,
    aes(x = time.norm, y = death.norm),
    shape = 13,
    size = 4
  ) +
  aes(color = country, fill = country) +
  ggtitle(title.prediction.norm)



########################################################################################################
p.prediction.log <- ggplot(death.prediction, aes(date, death)) +
  geom_point() +
  geom_line() +
  geom_line(data = death.prediction,
            aes(x = date, y = prediction),
            linetype = "dashed") +
  scale_color_manual(values = myPalette(length(countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(countries.of.interest))) +
  scale_x_date(
    date_breaks = "7 days",
    date_minor_breaks = "1 day",
    limits = c(min(death.prediction$date), max.date.pred)
  ) +
  geom_ribbon(
    data = death.prediction,
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.3
  ) +
  geom_point(
    data = pic.value.all,
    aes(x = date, y = peak.epidemic),
    shape = 24,
    size = 4,
    color = "darkred"
  ) +
  labs(y = "#deaths") +
  geom_point(
    data = confinement.date.top,
    aes(x = date, y = death),
    shape = 13,
    size = 4
  ) +
  aes(color = country, fill = country) +
  ggtitle(title.prediction) +
  scale_y_continuous(trans = "log2",
                     n.breaks = 16,
                     limits = c(1, y.lim.max))


########################################################################################################
p.prediction.log.norm <-
  ggplot(death.prediction, aes(time.norm, death.norm)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = myPalette(length(countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(countries.of.interest))) +
  geom_line(
    data = death.prediction,
    aes(x = time.norm, y = prediction.norm),
    linetype = "dashed"
  ) +
  labs(y = "#deaths (per millions inhabitants)") +
  ggtitle(title.prediction.norm) +
  geom_point(
    data = confinement.date.top,
    aes(x = time.norm, y = death.norm),
    shape = 13,
    size = 4
  ) +
  aes(color = country, fill = country) +
  scale_y_continuous(
    trans = "log2",
    #n.breaks = 16,
    breaks = 2 ^ (-6:9),
    limits = c(0.01, 350)
  )


### number of death per day
ind.top <- which(daily.death$country %in% countries.of.interest)
p.daily.death <- ggplot(daily.death[ind.top, ], aes(date, death)) +
  geom_line(aes(color = country)) +
  geom_point(aes(color = country)) +
  scale_color_manual(values = myPalette(length(countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(countries.of.interest))) +
  scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 day") +
  ggtitle(
    paste(
      format(max(daily.cumulative.death$date), format = "%Y %B %d"),
      ". Daily number of deaths.",
      "data from https://github.com/CSSEGISandData/COVID-19."
    )
  ) +
  ylab("death per day")


### plot layout
plot.absolute <-
  subplot(format.legend.ggplotly(ggplotly(p.prediction)),
          style(format.legend.ggplotly(ggplotly(p.prediction.log)), showlegend = FALSE),
          nrows = 2)
plot.norm <-
  subplot(format.legend.ggplotly(ggplotly(p.prediction.norm)),
          style(format.legend.ggplotly(ggplotly(p.prediction.log.norm)), showlegend = FALSE),
          nrows = 2)

plot.daily.death <- (ggplotly(p.daily.death))

##########
### USA
##########

usa.title.prediction <-
  paste(
    format(max(daily.cumulative.death$date), format = "%Y %B %d"),
    ". USA - The peak of the epidemic = triangle. ",
    "data from https://github.com/CSSEGISandData/COVID-19."
  )

usa.title.prediction.norm <-
  paste(
    format(max(daily.cumulative.death$date), format = "%Y %B %d"),
    "USA - Normalized data by population size of the state. Time 0 is ~2 deaths for 1 million.",
    "data from https://github.com/CSSEGISandData/COVID-19."
  )
###################################################################################################
usa.p.prediction <- ggplot(usa.death.prediction, aes(date, death)) +
  geom_point() +
  geom_line() +
  geom_line(data = usa.death.prediction,
            aes(x = date, y = prediction),
            linetype = "dashed") +
  scale_color_manual(values = myPalette(length(usa.countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(usa.countries.of.interest))) +
  scale_x_date(
    date_breaks = "7 days",
    date_minor_breaks = "1 day",
    limits = c(min(usa.death.prediction$date), max.date.pred)
  ) +
  scale_y_continuous(n.breaks = 15,
                     limits = c(0, usa.y.lim.max)) +
  geom_ribbon(
    data = usa.death.prediction,
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.3
  ) +
  geom_point(
    data = usa.pic.value.all,
    aes(x = date, y = peak.epidemic),
    shape = 24,
    size = 4,
    color = "darkred",
    show.legend = FALSE
  ) +
  labs(y = "#deaths") +
  aes(color = country, fill = country) +
  ggtitle(usa.title.prediction)


###################################################################################################
usa.p.prediction.norm <-
  ggplot(usa.death.prediction, aes(time.norm, death.norm)) +
  geom_point() +
  geom_line() +
  geom_line(
    data = usa.death.prediction,
    aes(x = time.norm, y = prediction.norm),
    linetype = "dashed"
  ) +
  scale_color_manual(values = myPalette(length(usa.countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(usa.countries.of.interest))) +
  labs(y = "#deaths (per millions inhabitants)") +
  scale_y_continuous(n.breaks = 15,
                     limits = c(0, 350)) +
  aes(color = country, fill = country) +
  ggtitle(usa.title.prediction.norm)

########################################################################################################
usa.p.prediction.log <- ggplot(usa.death.prediction, aes(date, death)) +
  geom_point() +
  geom_line() +
  geom_line(data = usa.death.prediction,
            aes(x = date, y = prediction),
            linetype = "dashed") +
  scale_color_manual(values = myPalette(length(usa.countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(usa.countries.of.interest))) +
  scale_x_date(
    date_breaks = "7 days",
    date_minor_breaks = "1 day",
    limits = c(min(usa.death.prediction$date), max.date.pred)
  ) +
  geom_ribbon(
    data = usa.death.prediction,
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.3
  ) +
  geom_point(
    data = usa.pic.value.all,
    aes(x = date, y = peak.epidemic),
    shape = 24,
    size = 4,
    color = "darkred"
  ) +
  labs(y = "#deaths") +
  aes(color = country, fill = country) +
  ggtitle(usa.title.prediction) +
  scale_y_continuous(trans = "log2",
                     n.breaks = 16,
                     limits = c(1, usa.y.lim.max))

########################################################################################################
usa.p.prediction.log.norm <-
  ggplot(usa.death.prediction, aes(time.norm, death.norm)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = myPalette(length(usa.countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(usa.countries.of.interest))) +
  geom_line(
    data = usa.death.prediction,
    aes(x = time.norm, y = prediction.norm),
    linetype = "dashed"
  ) +
  labs(y = "#deaths (per millions inhabitants)") +
  ggtitle(usa.title.prediction.norm) +
  aes(color = country, fill = country) +
  scale_y_continuous(
    trans = "log2",
    breaks = 2 ^ (-6:9),
    limits = c(0.01, 350)
  )

### number of death per day
usa.ind.top <- which(usa.daily.death$country %in% usa.countries.of.interest)
usa.p.daily.death <- ggplot(usa.daily.death[usa.ind.top, ], aes(date, death)) +
  geom_line(aes(color = country)) +
  geom_point(aes(color = country)) +
  scale_color_manual(values = myPalette(length(usa.countries.of.interest))) +
  scale_fill_manual(values = myPalette(length(usa.countries.of.interest))) +
  scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 day") +
  ggtitle(
    paste(
      format(max(usa.daily.cumulative.death$date), format = "%Y %B %d"),
      ". USA - Daily number of deaths.",
      "data from https://github.com/CSSEGISandData/COVID-19."
    )
  ) +
  ylab("death per day")

### plot layout
usa.plot.absolute <-
  subplot(format.legend.ggplotly(ggplotly(usa.p.prediction)),
          style(format.legend.ggplotly(ggplotly(usa.p.prediction.log)), showlegend = FALSE),
          nrows = 2)
usa.plot.norm <-
  subplot(format.legend.ggplotly(ggplotly(usa.p.prediction.norm)),
          style(format.legend.ggplotly(ggplotly(usa.p.prediction.log.norm)), showlegend = FALSE),
          nrows = 2)

usa.plot.daily.death <- (ggplotly(usa.p.daily.death))
