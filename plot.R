

myPalette <- colorRampPalette(brewer.pal(12, "Paired"))


############
### world
############

title.prediction <-
  paste(
    format(max(daily.cumulative.death$date), format = "%Y %B %d"),
    " - Observed and predicted values with confidence intervals. Estimated date of the peak of the epidemic = triangle. Confinement date = circle with an X. "
  )

title.prediction.norm <-
  paste(
    format(max(daily.cumulative.death$date), format = "%Y %B %d"),
    " - Normalized data by population size of the country. Time 0 is ~2 deaths for 1 million.",
    "data from https://github.com/CSSEGISandData/COVID-19."
  )

title.daily.death <- paste(
  format(max(daily.cumulative.death$date), format = "%Y %B %d"),
  ". Daily number of deaths.",
  "data from https://github.com/CSSEGISandData/COVID-19."
)

#########################################################################################
p.prediction <- plot.prediction(
  death.prediction = death.prediction,
  countries.of.interest = countries.of.interest,
  max.date.pred = max.date.pred,
  y.lim.max = y.lim.max,
  pic.value.all = pic.value.all,
  confinement.date = confinement.date.top,
  log = FALSE,
  title.prediction = title.prediction
)


#########################################################################################
p.prediction.norm <-
  plot.prediction.norm(
    death.prediction = death.prediction,
    countries.of.interest = countries.of.interest,
    confinement.date = confinement.date.top,
    title.prediction.norm = title.prediction.norm,
    log = FALSE
  )


#########################################################################################
p.prediction.log <-
  plot.prediction(
    death.prediction = death.prediction,
    countries.of.interest = countries.of.interest,
    max.date.pred = max.date.pred,
    y.lim.max = y.lim.max,
    pic.value.all = pic.value.all,
    confinement.date = confinement.date.top,
    log = TRUE,
    title.prediction = title.prediction
  )


#########################################################################################
p.prediction.log.norm <-
  plot.prediction.norm(
    death.prediction = death.prediction,
    countries.of.interest = countries.of.interest,
    confinement.date = confinement.date.top,
    title.prediction.norm = title.prediction.norm,
    log = TRUE
  )


### number of death per day
ind.top <- which(daily.death$country %in% countries.of.interest)
p.daily.death <-
  plot.daily.death(
    daily.death = daily.death[ind.top,],
    countries.of.interest = countries.of.interest,
    title.daily.death = title.daily.death
  )

htmlwidgets::saveWidget(p.prediction,
                        "absolute.linear.html",
                        selfcontained = FALSE,
                        libdir = "lib")

htmlwidgets::saveWidget(p.prediction.log,
                        "absolute.log.html",
                        selfcontained = FALSE,
                        libdir = "lib")

htmlwidgets::saveWidget(
  p.prediction.norm,
  "absolute.linear.norm.html",
  selfcontained = FALSE,
  libdir = "lib"
)

htmlwidgets::saveWidget(
  p.prediction.log.norm,
  "absolute.log.norm.html",
  selfcontained = FALSE,
  libdir = "lib"
)

htmlwidgets::saveWidget(p.daily.death,
                        "daily.html",
                        selfcontained = FALSE,
                        libdir = "lib")

##########
### USA
##########

usa.title.prediction <-
  paste(
    format(max(daily.cumulative.death$date), format = "%Y %B %d"),
    " - USA - Observed and predicted values with confidence intervals. Estimated date of the peak of the epidemic = triangle. Confinement date = circle with an X. "
  )

usa.title.prediction.norm <-
  paste(
    format(max(daily.cumulative.death$date), format = "%Y %B %d"),
    "USA - Normalized data by population size of the state. Time 0 is ~2 deaths for 1 million.",
    "data from https://github.com/CSSEGISandData/COVID-19."
  )

usa.title.daily.death <- paste(
  format(max(daily.cumulative.death$date), format = "%Y %B %d"),
  ". USA - Daily number of deaths.",
  "data from https://github.com/CSSEGISandData/COVID-19."
)

#########################################################################################
usa.p.prediction <-
  plot.prediction(
    death.prediction = usa.death.prediction,
    countries.of.interest = usa.countries.of.interest,
    max.date.pred = max.date.pred,
    y.lim.max = usa.y.lim.max,
    pic.value.all = usa.pic.value.all,
    confinement.date = NULL,
    log = FALSE,
    title.prediction = usa.title.prediction
  )


#########################################################################################
usa.p.prediction.norm <-
  plot.prediction.norm(
    death.prediction = usa.death.prediction,
    countries.of.interest = usa.countries.of.interest,
    confinement.date = NULL,
    title.prediction.norm = usa.title.prediction.norm,
    log = FALSE
  )

#########################################################################################
usa.p.prediction.log <-
  plot.prediction(
    death.prediction = usa.death.prediction,
    countries.of.interest = usa.countries.of.interest,
    max.date.pred = max.date.pred,
    y.lim.max = usa.y.lim.max,
    pic.value.all = usa.pic.value.all,
    confinement.date = NULL,
    log = TRUE,
    title.prediction = usa.title.prediction
  )


#########################################################################################
usa.p.prediction.log.norm <-
  plot.prediction.norm(
    death.prediction = usa.death.prediction,
    countries.of.interest = usa.countries.of.interest,
    confinement.date = NULL,
    title.prediction.norm = usa.title.prediction.norm,
    log = TRUE
  )

### number of death per day
usa.ind.top <-
  which(usa.daily.death$country %in% usa.countries.of.interest)


usa.p.daily.death <-
  plot.daily.death(
    daily.death = usa.daily.death[usa.ind.top,],
    countries.of.interest = usa.countries.of.interest,
    title.daily.death = usa.title.daily.death
  )

htmlwidgets::saveWidget(
  usa.p.prediction,
  "usa.absolute.linear.html",
  selfcontained = FALSE,
  libdir = "lib"
)

htmlwidgets::saveWidget(
  usa.p.prediction.log,
  "usa.absolute.log.html",
  selfcontained = FALSE,
  libdir = "lib"
)

htmlwidgets::saveWidget(
  usa.p.prediction.norm,
  "usa.absolute.linear.norm.html",
  selfcontained = FALSE,
  libdir = "lib"
)

htmlwidgets::saveWidget(
  usa.p.prediction.log.norm,
  "usa.absolute.log.norm.html",
  selfcontained = FALSE,
  libdir = "lib"
)

htmlwidgets::saveWidget(usa.p.daily.death,
                        "usa.daily.html",
                        selfcontained = FALSE,
                        libdir = "lib")
