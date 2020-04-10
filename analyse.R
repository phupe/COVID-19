require(plotly)
require(ggplot2)
require(scales)
require(nlstools)
require(investr)
require(nls2)
require(propagate)
require(minpack.lm)
require(RColorBrewer)
require(htmlwidgets)

rm(list = ls())

source("utils.R")

#########################################
### Addtional information
#########################################

### confinement dates
# rep tcheque 13 mars
# nouvelle zealand 23 mars
# malaisie 18 mars
# afrique du sud 27 mars
# zimbawe 27 mars
# Tunisia 20 mars
# Marco 21 mars
# salvador 21 mars
# colombie 24 mars
# venezuela 17 mars
# bolivia 22 mars
# argentine 19 mars
confinement.date <- NULL
confinement.date <-
  rbind(confinement.date,
        data.frame(
          country = "Austria",
          date = as.Date("03/15/20", format = "%m/%d/%y")
        ))
confinement.date <-
  rbind(confinement.date,
        data.frame(
          country = "Belgium",
          date = as.Date("03/18/20", format = "%m/%d/%y")
        ))
confinement.date <-
  rbind(confinement.date, data.frame(
    country = "China",
    date = as.Date("01/22/20", format = "%m/%d/%y")
  ))
confinement.date <-
  rbind(confinement.date,
        data.frame(
          country = "Denmark",
          date = as.Date("03/13/20", format = "%m/%d/%y")
        ))
confinement.date <-
  rbind(confinement.date,
        data.frame(
          country = "France",
          date = as.Date("03/17/20", format = "%m/%d/%y")
        ))
confinement.date <-
  rbind(confinement.date,
        data.frame(
          country = "Greece",
          date = as.Date("03/23/20", format = "%m/%d/%y")
        ))
confinement.date <-
  rbind(confinement.date, data.frame(
    country = "India",
    date = as.Date("03/24/20", format = "%m/%d/%y")
  ))
confinement.date <-
  rbind(confinement.date, data.frame(
    country = "Italy",
    date = as.Date("03/08/20", format = "%m/%d/%y")
  ))
confinement.date <-
  rbind(confinement.date,
        data.frame(
          country = "Portugal",
          date = as.Date("03/19/20", format = "%m/%d/%y")
        ))
confinement.date <-
  rbind(confinement.date,
        data.frame(
          country = "Romania",
          date = as.Date("03/25/20", format = "%m/%d/%y")
        ))
#confinement.date <- rbind(confinement.date, data.frame(country = "Russia", date = as.Date("03/30/20", format="%m/%d/%y")))
confinement.date <-
  rbind(confinement.date, data.frame(
    country = "Spain",
    date = as.Date("03/14/20", format = "%m/%d/%y")
  ))
confinement.date <-
  rbind(confinement.date, data.frame(
    country = "UK",
    date = as.Date("03/23/20", format = "%m/%d/%y")
  ))


##############################################
### source data from:
### https://github.com/CSSEGISandData/COVID-19
###############################################

data.dir <- "data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/"
covid.file <-
  paste(data.dir, "time_series_covid19_deaths_global.csv", sep = "")

covid.us.file <-
  paste(data.dir, "time_series_covid19_deaths_US.csv", sep = "")

### data for all countries over the world
world.data <- make.covid.agg(covid.file = covid.file)
daily.cumulative.death <- world.data[["daily.cumulative.death"]]
total.death <- world.data[["total.death"]]
daily.death <- world.data[["daily.death"]]


### data for all states over USA
usa.data <- make.covid.agg(covid.file = covid.us.file, group = "Province_State")
usa.daily.cumulative.death <- usa.data[["daily.cumulative.death"]]
usa.total.death <- usa.data[["total.death"]]
usa.daily.death <- usa.data[["daily.death"]]

### Adjust data for France as ehpad cases were not counted until 04/02/20"
france.adjusted <- adjust.france(daily.cumulative.death[which(daily.cumulative.death$country == "France"),])
# daily.cumulative.death <- rbind(daily.cumulative.death, france.adjusted)


#####################################
### population number for countries
#####################################
world.population.file <-
  "data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.tsv"
country.population <-
  make.world.population(file = world.population.file)
rownames(country.population) <- country.population$country

country.no.pop.size <-
  setdiff(unique(daily.cumulative.death$country),
          country.population$country)

if (length(country.no.pop.size) > 0)
{
  daily.cumulative.death <-
    daily.cumulative.death[-which(daily.cumulative.death$country %in% country.no.pop.size), ]
}

######################################
### population number for USA states
######################################

usa.population.file <-
  "data/nst-est2019-01.tsv"
usa.population <-
  make.usa.population(file = usa.population.file) 
rownames(usa.population) <- usa.population$country

usa.daily.cumulative.death <- usa.daily.cumulative.death[which(usa.daily.cumulative.death$country %in% usa.population$country),]


### number of deaths all over the world
dd.world <-
  aggregate(daily.cumulative.death["death"],
            by = list(date = daily.cumulative.death$date),
            sum)
dd.world$time <- as.numeric(dd.world$date)
dd.world$time <- dd.world$time - min(dd.world$time)

### additional information
confinement.date <- merge(confinement.date, daily.cumulative.death)
confinement.date$time <- as.numeric(confinement.date$date)
confinement.date$time.norm <- NA
confinement.date <- merge(confinement.date, country.population)
confinement.date$death.norm <-
  confinement.date$death / confinement.date$population * 10 ^ 6
rownames(confinement.date) <- confinement.date$country

############
### World
############

nb.countries <- 3

### Top countries
top.countries <- total.death$country[1:nb.countries]

### countries of interest
countries.of.interest <- unique(c(top.countries, "Korea, South", "Japan"))

###########
### USA
###########

### Top USA states
usa.top.countries <- usa.total.death$country[1:nb.countries]

### USA states of interest
usa.countries.of.interest <- usa.top.countries

y.lim.max <- 32500
usa.y.lim.max <- 11000
max.date.pred <- as.Date("05/01/20", format = "%m/%d/%y")

###################################
### prediction model (world)
###################################
res.prediction <-
  fitModel(
    daily.cumulative.death = daily.cumulative.death,
    country.population = country.population,
    countries.of.interest = countries.of.interest,
    confinement.date = confinement.date,
    y.lim.max = y.lim.max,
    max.date.pred = max.date.pred
  )

death.prediction <- res.prediction[["death.prediction"]]
confint.all <- res.prediction[["confint.all"]]
pic.value.all <- res.prediction[["pic.value.all"]]
confinement.date <- res.prediction[["confinement.date"]]

confinement.date.top <-
  confinement.date[which(confinement.date$country %in% countries.of.interest), ]

###################################
### prediction model (USA)
###################################
usa.confinement.date <- NULL
usa.res.prediction <-
  fitModel(
    daily.cumulative.death = usa.daily.cumulative.death,
    country.population = usa.population,
    countries.of.interest = usa.countries.of.interest,
    confinement.date = usa.confinement.date,
    y.lim.max = usa.y.lim.max,
    max.date.pred = max.date.pred
  )

usa.death.prediction <- usa.res.prediction[["death.prediction"]]
usa.confint.all <- usa.res.prediction[["confint.all"]]
usa.pic.value.all <- usa.res.prediction[["pic.value.all"]]
usa.confinement.date <- usa.res.prediction[["confinement.date"]]

###################################
### graphics
###################################

source("plot.R")

