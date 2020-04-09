##########################
### format the data
##########################


make.world.population <- function(file = NULL, country)
{
  world.population <- read.delim(
    file = file,
    sep = "\t",
    head = TRUE,
    stringsAsFactors = FALSE
  )
  
  world.population <- data.frame(
    country = world.population[, 3],
    population = world.population$X2020,
    stringsAsFactors = FALSE
  )
  world.population$population <-
    gsub(" ", "", world.population$population)
  world.population$population <-
    as.integer(world.population$population) * 1000
  
  world.population$country[which(world.population$country == "Bolivia (Plurinational State of)")] <-
    "Bolivia"
  world.population$country[which(world.population$country == "Brunei Darussalam")] <-
    "Brunei"
  world.population$country[which(world.population$country == "Myanmar")] <-
    "Burma"
  world.population$country[which(world.population$country == "Congo")] <-
    "Congo (Brazzaville)"
  world.population$country[which(world.population$country == "Democratic Republic of the Congo")] <-
    "Congo (Kinshasa)"
  world.population$country[which(world.population$country == "Côte d'Ivoire")] <-
    "Cote d'Ivoire"
  world.population$country[which(world.population$country == "Iran (Islamic Republic of)")] <-
    "Iran"
  world.population$country[which(world.population$country == "Republic of Korea")] <-
    "Korea, South"
  world.population$country[which(world.population$country == "Lao People's Democratic Republic")] <-
    "Laos"
  world.population$country[which(world.population$country == "Republic of Moldova")] <-
    "Moldova"
  world.population$country[which(world.population$country == "Russian Federation")] <-
    "Russia"
  world.population$country[which(world.population$country == "Syrian Arab Republic")] <-
    "Syria"
  world.population$country[which(world.population$country == "United States of America")] <-
    "US"
  world.population$country[which(world.population$country == "United Republic of Tanzania")] <-
    "Tanzania"
  world.population$country[which(world.population$country == "China, Taiwan Province of China")] <-
    "Taiwan*"
  world.population$country[which(world.population$country == "Venezuela (Bolivarian Republic of)")] <-
    "Venezuela"
  world.population$country[which(world.population$country == "Viet Nam")] <-
    "Vietnam"
  
  return(world.population)
}

make.usa.population <- function(file = NULL, country)
{
  usa.population <- read.delim(
    file = file,
    sep = "\t",
    head = TRUE,
    stringsAsFactors = FALSE
  )
  
  usa.population <- data.frame(
    country = usa.population[, 1],
    population = usa.population$X2019,
    stringsAsFactors = FALSE
  )
  
  return(usa.population)
}

make.covid.agg <-
  function(covid.file = NULL, group = "Country.Region")
  {
    ###############################################
    ### the file contains columns for each date ###
    ### we want a date.frame with 3 colums:     ###
    ###    - country                            ###
    ###    - date                               ###
    ###    - death                              ###
    ###############################################
    
    ### file header
    covid.header <-
      read.delim(
        file = covid.file,
        header = FALSE,
        stringsAsFactors = FALSE,
        sep = ",",
        nrow = 1
      )
    
    ### death value
    covid <-
      read.delim(
        file = covid.file,
        header = TRUE,
        stringsAsFactors = FALSE,
        sep = ","
      )
    ### list of columns to delete
    ind.del <- grep("^X[0-9]", names(covid), invert = TRUE)
    
    ### aggregate values by country
    covid.agg <-
      rowsum(covid[, -ind.del], group = covid[, group])
    
    ### date (MM/DD/YYYY)
    covid.date <- t(covid.header[1, -ind.del])
    
    ######################################################################################
    ### data.frame with country, date, death with number of death per day (cumul per day)
    ######################################################################################
    
    country.value <- NULL
    date.value <- NULL
    death.value <- NULL
    
    for (i in 1:nrow(covid.agg))
    {
      country.value <-
        c(country.value, rep(rownames(covid.agg)[i], ncol(covid.agg)))
      date.value <- c(date.value, covid.date)
      death.value <-
        c(death.value, as.vector(t(covid.agg[i,])))
    }
    
    daily.cumulative.death <- data.frame(
      country = country.value,
      date = date.value,
      death = death.value,
      stringsAsFactors = FALSE
    )
    
    daily.cumulative.death$date <-
      as.Date(daily.cumulative.death$date, format = "%m/%d/%y")
    
    ##########################################################################################
    ### data.frame with country, date, death with number of death per day (increase per day)
    ##########################################################################################
    
    country.value <- NULL
    date.value <- NULL
    death.value <- NULL
    
    for (i in 1:nrow(covid.agg))
    {
      country.value <-
        c(country.value, rep(rownames(covid.agg)[i], ncol(covid.agg) - 1))
      date.value <- c(date.value, covid.date[-1])
      death.value <-
        c(death.value, diff(as.vector(t(covid.agg[i,]))))
    }
    
    daily.death <- data.frame(
      country = country.value,
      date = date.value,
      death = death.value,
      stringsAsFactors = FALSE
    )
    
    daily.death$date <-
      as.Date(daily.death$date, format = "%m/%d/%y")
    
    ### Number of death at t0
    daily.death.t0 <- data.frame(
      country = rownames(covid.agg),
      date = covid.date[1],
      death.t0 = covid.agg[, 1],
      stringsAsFactors = FALSE
    )
    
    ### Total number of deaths per country
    total.death <-
      rowsum(daily.death[, 3], group = daily.death$country)
    total.death <-
      data.frame(country = rownames(total.death), death = total.death)
    total.death <-
      merge(daily.death.t0[, c("country", "death.t0")], total.death)
    total.death$death.total <-
      total.death$death.t0 + total.death$death
    total.death <-
      total.death[order(total.death$death.total, decreasing = TRUE),]
    
    return(
      list(
        daily.cumulative.death = daily.cumulative.death,
        total.death = total.death,
        daily.death = daily.death
      )
    )
    
  }



### adjust data for France
adjust.france <- function(daily.cumulative.death = NULL,
                          date.ehpad = as.Date("04/02/20", format = "%m/%d/%y"),
                          scale = 1.3)
{
  ind <- which(daily.cumulative.death$date < date.ehpad)
  daily.cumulative.death$death[ind] <-
    daily.cumulative.death$death[ind] * scale
  daily.cumulative.death$death[ind] <-
    as.integer(floor(daily.cumulative.death$death[ind]))
  
  daily.cumulative.death$country <- "France (adjusted)"
  return(daily.cumulative.death)
}


############################
### ggplotly plot formatting
############################


add.prediction.button <- function(gg.plotly.fig = NULL,
                                  countries.of.interest = NULL)
{
  nb.obj <- length(gg.plotly.fig$x$data)
  nb.countries <- length(countries.of.interest)
  show.prediction <- rep(TRUE, nb.obj)
  hide.prediction <- rep(FALSE, nb.obj)
  hide.prediction[1:nb.countries] <- TRUE
  
  gg.plotly.fig <- gg.plotly.fig %>%
    layout(updatemenus = list(list(
      x = 0.1,
      y = 0.95 ,
      buttons = list(
        list(
          method = "restyle",
          args = list("visible", as.list(show.prediction)),
          label = "Prediction"
        ),
        
        list(
          method = "restyle",
          args = list("visible", as.list(hide.prediction)),
          label = "Raw data"
        )
      )
    )))
  
  return(gg.plotly.fig)
}

format.legend.ggplotly <- function(gg.plotly.fig = NULL)
{
  for (i in 1:length(gg.plotly.fig$x$data))
  {
    if (length(grep("marker", names(gg.plotly.fig$x$data[i][[1]]))) > 0)
    {
      if (gg.plotly.fig$x$data[i][[1]]$marker$symbol == "triangle-up")
      {
        cat("legend FALSE\n")
        gg.plotly.fig$x$data[[i]]$showlegend <- FALSE
      }
    }
  }
  return(gg.plotly.fig)
}


#########################################################################################
plot.prediction <- function(death.prediction = NULL,
                            countries.of.interest = NULL,
                            max.date.pred = NULL,
                            y.lim.max = NULL,
                            pic.value.all = NULL,
                            confinement.date = NULL,
                            log = FALSE,
                            title.prediction = NULL)
{
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
    geom_ribbon(data = death.prediction,
                aes(ymin = lower,
                    ymax = upper),
                alpha = 0.3) +
    geom_point(
      data = pic.value.all,
      aes(x = date, y = peak.epidemic),
      shape = 24,
      size = 4,
      color = "darkred",
      show.legend = FALSE
    )
  
  if (is.null(confinement.date) == FALSE)
  {
    p.prediction <- p.prediction + geom_point(
      data = confinement.date,
      aes(x = date, y = death),
      shape = 13,
      size = 4
    )
  }
  
  if (log == FALSE)
  {
    p.prediction <- p.prediction + scale_y_continuous(n.breaks = 15,
                                                      limits = c(0, y.lim.max))
    
  } else
  {
    p.prediction <- p.prediction + scale_y_continuous(
      trans = "log2",
      n.breaks = 16,
      limits = c(1, y.lim.max)
    )
  }
  
  p.prediction <- p.prediction + labs(y = "#deaths") +
    aes(color = country, fill = country) +
    ggtitle(title.prediction)
  
  p.prediction <- ggplotly(p.prediction)
  p.prediction <- format.legend.ggplotly(p.prediction)
  p.prediction <-
    add.prediction.button(p.prediction, countries.of.interest)
  return(p.prediction)
}


#########################################################################################
plot.prediction.norm <- function(death.prediction = NULL,
                                 countries.of.interest = NULL,
                                 confinement.date = NULL,
                                 title.prediction.norm = NULL,
                                 log = FALSE)
{
  p.prediction.norm <-
    ggplot(death.prediction, aes(time.norm, death.norm)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = myPalette(length(countries.of.interest))) +
    scale_fill_manual(values = myPalette(length(countries.of.interest))) +
    geom_line(data = death.prediction,
              aes(x = time.norm, y = prediction.norm),
              linetype = "dashed") +
    labs(x = "normalized time (day)",
         y = "#deaths (per millions inhabitants)") +
    ggtitle(title.prediction.norm)
  if (is.null(confinement.date) == FALSE)
  {
    p.prediction.norm <- p.prediction.norm + geom_point(
      data = confinement.date,
      aes(x = time.norm, y = death.norm),
      shape = 13,
      size = 4
    )
  }
  
  if (log == FALSE)
  {
    p.prediction.norm <-
      p.prediction.norm + scale_y_continuous(n.breaks = 15,
                                             limits = c(0, 350))
  } else
  {
    p.prediction.norm <- p.prediction.norm + scale_y_continuous(
      trans = "log2",
      breaks = 2 ^ (-6:9),
      limits = c(0.01, 350)
    )
  }
  
  p.prediction.norm <-
    p.prediction.norm + aes(color = country, fill = country)
  p.prediction.norm <- ggplotly(p.prediction.norm)
  p.prediction.norm <- format.legend.ggplotly(p.prediction.norm)
  
  return(p.prediction.norm)
}



###############################################################################
plot.daily.death <- function(daily.death = NULL,
                             countries.of.interest = NULL,
                             title.daily.death = NULL)
{
  p.daily.death <- ggplot(daily.death, aes(date, death)) +
    geom_line(aes(color = country)) +
    geom_point(aes(color = country)) +
    scale_color_manual(values = myPalette(length(countries.of.interest))) +
    scale_fill_manual(values = myPalette(length(countries.of.interest))) +
    scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 day") +
    ggtitle(title.daily.death) +
    ylab("death per day")
  
  return(ggplotly(p.daily.death))
}


### modify the values to have better rendering in ggplot graphics
### for the confidence intervals

adjust.confint.4plot <- function(l.u.int = NULL,
                                death.country = NULL,
                                y.lim.max = NULL)
{
    ind.upper <- which(l.u.int$upper > y.lim.max)
    if (length(ind.upper) == nrow(death.country))
    {
      l.u.int$upper[ind.upper] <- NA
    } else
    {
      l.u.int$upper[ind.upper] <- y.lim.max
    }
    ind.lower <- which(l.u.int$lower < 0)
    if (length(ind.lower) == nrow(death.country))
    {
      l.u.int$lower[ind.lower] <- NA
    } else
    {
      l.u.int$lower[ind.lower] <- 1
    }
    if (length(ind.lower) == nrow(death.country) |
        length(ind.upper) == nrow(death.country))
    {
      l.u.int$lower <- l.u.int$upper <- NA
    }
    ind.inconsistent <- which(l.u.int$lower > l.u.int$upper)
    if (length(ind.inconsistent) > 0)
    {
      l.u.int$lower[ind.inconsistent] <- max(l.u.int$upper)
    }
    death.country <- cbind(death.country, l.u.int)

    return(death.country)

}
##########################
### statistical analysis
##########################

### fit logistic model

fitLogistic <- function(daily.cumulative.death = NULL,
                        country.population = NULL,
                        countries.of.interest = NULL,
                        confinement.date = NULL,
                        y.lim.max = 30000,
                        max.date.pred = as.Date("05/01/20", format = "%m/%d/%y"))
{
  ### max.date.pres = max date for prediction
  ### y.lim.max = maximal y value that will be displayed on the plot
  
  ### variable to store the prediction
  death.prediction <- NULL
  
  ### minimal time (when date are converted to integer)
  min.time <- min(as.numeric(daily.cumulative.death$date))
  
  ### variable to store the confidence interval of the estimated parameters for all countries
  confint.all <- list()
  
  ### variable to store the prediction of the date of the epidemic peak for all countries
  pic.value.all <- NULL
  
  
  ### fit logistic model
  for (country in countries.of.interest)
  {
    cat("\n")
    ind <- which(daily.cumulative.death$country == country)
    death.country <- daily.cumulative.death[ind, ]
    ind.0 <- which(death.country$death <= 5)
    if (length(ind.0) > 0)
    {
      death.country <- death.country[-ind.0, ]
    }
    death.country$time <- as.numeric(death.country$date)
    death.country$time <- death.country$time - min.time
    
    ### number of observation
    nb.obs <- nrow(death.country)
    
    cat(country,
        "- number of values:",
        nrow(death.country),
        "\n")
    
    ### sigmoid function
    params.grid <-
      expand.grid(
        Asym = c(100, seq(1000, 30000, 500)),
        xmid = seq(10, 100, 2),
        scal = seq(2, 8, 0.5)
      )
    cat("fit with brute force\n")
    fit.res.logis0 <-
      nls2(
        death ~ Asym / ((1 + exp((xmid - time) / scal
        ))),
        data = death.country,
        algorithm = "brute-force",
        start = params.grid
      )
    Alogis0 <- coefficients(fit.res.logis0)["Asym"]
    xmid0 <- coefficients(fit.res.logis0)["xmid"]
    scal0 <- coefficients(fit.res.logis0)["scal"]
    cat("fit with previous model as start value\n")
    fit.res.logis <-
      nlsLM(
        death ~ Asym / ((1 + exp((xmid - time) / scal
        ))),
        data = death.country,
        start = list(
          Asym = Alogis0,
          xmid = xmid0,
          scal = scal0
        ),
        control = nls.lm.control(maxiter = 1024, maxfev = 20000),
        model = TRUE
      )
    Alogis <- coef(fit.res.logis)["Asym.Asym"]
    xmid <- coef(fit.res.logis)["xmid.xmid"]
    scal <- coef(fit.res.logis)["scal.scal"]
    cat("\tAsym: ", Alogis, " xmid: ", xmid, " scal: ", scal, "\n")
    
    confint.value <- as.matrix(confint2(fit.res.logis))
    confint.pic.value <-
      min(daily.cumulative.death$date) + floor(confint.value["xmid.xmid", ]) + 1
    pic.value <-
      min(daily.cumulative.death$date) + floor(xmid) + 1
    
    confint.all[[country]] <-
      list(confint = confint.value,
           confint.pic = confint.pic.value,
           pic.value)
    
    extra.date <-
      seq.Date(
        from = max(death.country$date) + 1,
        to = as.Date(max.date.pred, format = "%m/%d/%y"),
        by = "day"
      )
    extra.value <-
      data.frame(
        country = rep(death.country$country[1], length(extra.date)),
        date = extra.date,
        death = NA,
        time = as.numeric(extra.date) - min.time,
        stringsAsFactors = FALSE
      )
    if (nrow(death.country) > 20)
    {
      death.country <- rbind(death.country, extra.value)
    }
    
    death.country$prediction <-
      floor(Alogis / (1 + exp((
        xmid - death.country$time
      ) / scal)))
    
    cat("condidence interval\n")
    l.u.int.values <-
      my.predictNLS(fit.res.logis,
                    death.country["time"],
                    interval = "confidence",
                    do.sim = FALSE)
    l.u.int <-
      data.frame(
        lower = floor(l.u.int.values$summary$Prop.2.5),
        upper = floor(l.u.int.values$summary$Prop.97.5)
      )
    
    ### modify the values to have better rendering in ggplot graphics
    death.country <- adjust.confint.4plot(l.u.int = l.u.int,
                                          death.country = death.country,
                                          y.lim.max = y.lim.max)
    
    ### normalize time with a number of deaths equals to deaths.ref
    deaths.ref <-
      2 *  country.population[country, "population"] / 10 ^ 6
    death.country <- death.country[order(death.country$date), ]
    
    if (deaths.ref < min(death.country$death, na.rm = TRUE))
    {
      time.norm <- death.country$time[1]
    } else
    {
      ind.ref1 <- max(which(death.country$death < deaths.ref))
      ind.ref2 <- ind.ref1 + 1
      if (ind.ref2 > nb.obs)
      {
        time.norm <- max(death.country$time)
      } else
      {
        y1 <- death.country$death[ind.ref1]
        y2 <- death.country$death[ind.ref2]
        x1 <- death.country$time[ind.ref1]
        x2 <- death.country$time[ind.ref2]
        a.slope <- (y2 - y1) / (x2 - x1)
        b.coef <- y1 - a.slope * x1
        time.norm <- (deaths.ref - b.coef) / a.slope
      }
    }
    
    death.country$time.norm <- death.country$time - time.norm
    
    confinement.date[country, "time.norm"] <-
      confinement.date[country, "time"] - time.norm - min.time
    
    ### normalize with the popupalion size
    population.scale <-
      10 ^ 6 / country.population[country, "population"]
    if (is.na(population.scale))
    {
      stop(paste("ERROR: population is not available for country = ",
                 country))
    } else
    {
      death.country$prediction.norm <-
        death.country$prediction * population.scale
      death.country$death.norm <-
        death.country$death * population.scale
    }
    
    death.prediction <- rbind(death.prediction, death.country)
    
    if (pic.value <= max.date.pred)
    {
      ind.pic <- which(death.country$date == pic.value)
      peak.epidemic <- death.country$prediction[ind.pic]
    } else
    {
      peak.epidemic <- death.country$prediction[max.date.pred]
    }
    
    if (nrow(death.country) > 20)
    {
      pic.value.df <-
        data.frame(
          country = country,
          date = pic.value,
          peak.epidemic = peak.epidemic,
          stringsAsFactors = FALSE
        )
      pic.value.all <- rbind(pic.value.all, pic.value.df)
    }
  }
  
  return(
    list(
      death.prediction = death.prediction,
      confint.all = confint.all,
      pic.value.all = pic.value.all,
      confinement.date = confinement.date
    )
  )
  
}

### confidence interval for from nlsLM model
my.predictNLS <-
  function (model,
            newdata,
            newerror,
            interval = c("confidence",
                         "prediction", "none"),
            alpha = 0.05,
            ...)
  {
    interval <- match.arg(interval)
    RHS <- as.list(eval(model$call$formula))[[3]]
    EXPR <- as.expression(RHS)
    VARS <- all.vars(EXPR)
    COEF <- coef(model)
    names(COEF) <- gsub("\\..*", "", names(COEF))
    predVAR <- setdiff(VARS, names(COEF))
    if (missing(newdata)) {
      newdata <- try(eval(model$data)[, predVAR, drop = FALSE],
                     silent = TRUE)
      if (inherits(newdata, "try-error")) {
        newdata <- sapply(predVAR, function(i)
          get(i))
        names(newdata) <- predVAR
      }
    }
    if (length(setdiff(colnames(newdata), predVAR)) != 0)
      stop("predictNLS: 'newdata' should have column name(s): ",
           predVAR,
           "!\n")
    if (!missing(newerror)) {
      if (length(setdiff(colnames(newerror), predVAR)) != 0)
        stop("predictNLS: 'newerror' should have column name(s): ",
             predVAR,
             "!\n")
    }
    VCOV <- vcov(model)
    if (missing(newerror)) {
      newerror <- newdata
      newerror[] <- 0
    }
    else {
      if (!identical(dim(newdata), dim(newerror)))
        stop("'newdata' and 'newerror' should have the same dimensions!")
      if (!identical(colnames(newdata), colnames(newerror)))
        stop("'newdata' and 'newerror' should have the same column names!")
    }
    NR <- NROW(newdata)
    outMAT <- matrix(nrow = NR, ncol = 12)
    propLIST <- vector("list", length = NR)
    if (interval == "prediction") {
      form <- formula(model)
      form <-
        as.formula(paste(form[2], form[1], paste(form[3],
                                                 " + rv", sep = ""), sep = " "))
      EXPR <- as.expression(form[[3]])
    }
    for (i in 1:NR) {
      message("predictNLS: ",
              paste0("Propagating predictor value #",
                     i, "..."))
      tempDATA <- newdata[i, , drop = FALSE]
      errorDATA <- newerror[i, , drop = FALSE]
      DAT <- as.numeric(c(COEF, tempDATA))
      names(DAT) <- c(names(COEF), predVAR)
      DAT <- rbind(DAT, 0)
      row.names(DAT) <- NULL
      if (interval == "prediction")
        DAT <- cbind(DAT, rv = c(0, 0))
      if (interval == "confidence")
        COV <- mixCov(VCOV, errorDATA ^ 2)
      else {
        r <- residuals(model)
        w <- weights(model)
        rss <- sum(if (is.null(w))
          r ^ 2
          else
            r ^ 2 * w)
        n <- length(residuals(model))
        p <- length(coef(model))
        rv <- rss / (n - p)
        COV <- mixCov(VCOV, errorDATA ^ 2, rv)
      }
      DF <- df.residual(model)
      rownames(COV) <- gsub("\\..*", "", rownames(COV))
      colnames(COV) <- gsub("\\..*", "", colnames(COV))
      PROP <- propagate(
        expr = EXPR,
        data = DAT,
        cov = COV,
        alpha = alpha,
        df = DF,
        ...
      )
      propLIST[[i]] <- PROP
      outPROP <- PROP$prop
      outSIM <- PROP$sim
      OUT <- c(outPROP, outSIM)
      outMAT[i, ] <- OUT
    }
    outMAT <- as.data.frame(outMAT)
    colnames(outMAT) <-
      c(paste("Prop.", names(outPROP), sep = ""),
        paste("Sim.", names(outSIM), sep = ""))
    
    return(list(summary = outMAT, prop = propLIST))
  }
