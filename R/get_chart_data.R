library(dplyr)
library(tidyr)
library(r2d3)
library(jsonlite)
library(stringr)
library(purrr)
library(data.table)
library(zoo)
library(robservable)

library(glue)
library(RSQLite)

source("R/db_interface.R")

#' Get an Observable chart.
#'
#' Retrieves data from SQLite, processes it, and generates an Observable chart.
#'
#' @param dtst The dataset name.
#'
#' @return A list containing processed data, chart options, metadata,
#'   the Observable chart, and update information.
get_obs_chart <- function(dtst) {
    pp <- list()  # Parameters list
    
    dat <- get_data_for_chart(dtst, "data/sol_v4.db")
    
    # Parse JSON parameters and metadata options
    pp <- if (!is.na(dat$m$pp)) fromJSON(dat$m$pp) else list()
    mopts <- if (!is.na(dat$m$mopts)) fromJSON(dat$m$mopts) else list()
    
    # Data ordering
    if ("orderctg" %in% names(pp)) dat$d <- orderctg(dat$d, pp$orderctg)
    if ("orderxd" %in% names(pp)) dat$d <- orderxd(dat$d, pp$orderxd) 
    
    chart_opts <- c("chrt")
    
    # Separate xd column for text wrapping
    if ("wrap_text" %in% names(pp)) {
        dat$d <- dat$d |>
            separate(xd, sep = "(?<=[-])", remove = FALSE,
                     into = c("upper_xd", "lower_xd"))
    }
    
    # Chart options
    tickf <- ifelse(dat$m$tickformat %in% "%", ".0%",
                    ifelse(is.na(dat$m$tickformat), ".1f", dat$m$tickformat))
    ctype <- ifelse(dat$m$bar == 1, "bar", "line")
    
    optsx <- list(
        ytickformat = tickf,
        type = dat$m$type,
        charttype = ctype,
        stack = dat$m$stack,
        high = dat$m$highlight
    )
    
    if ("quarters" %in% names(dat$m)) optsx$quarters <- TRUE
    
    # Parse and add additional chart options
    opts <- if (!is.na(dat$m$opts)) fromJSON(dat$m$opts) else list()
    for (n in names(opts)) {
        print(glue("ADDING: {n} = {opts[[n]]}"))
        optsx[[n]] <- opts[[n]]
    }
    
    # Stack options
    if (dat$m$stack %in% TRUE) {
        optsx$stack <- TRUE
        optsx$stackgroup <- "stack"
        if (!"silent_x" %in% names(opts) && dat$m$type %in% c("date", "quarter")) {
            optsx$silent_x <- TRUE
        }
        optsx$convert_wide <- TRUE
    } else {
        optsx$stackgroup <- "group"
        optsx$stack <- FALSE
    }
    
    # Metadata options
    mopts$ttl <- mopts$title <- dat$m$charttitle # Combined assignment
    mopts$includetitles <- FALSE
    mopts$sub <- dat$m$subtitle
    mopts$sol_chapter <- dat$m$sol_chapter
    mopts$sol_sub <- dat$m$sol_subsection
    mopts$source <- dat$m$source
    mopts$link <- dat$m$link
    
    # Filter chart options
    if ("filter_chart" %in% names(pp) && pp$filter_chart == "keep") {
        chart_opts <- c("viewof luk", "chrt")
        # button_opts <- unique(dat$d$chart) # Not used, can be removed
    }
    
    # Handle single b value
    if (length(unique(dat$d$b)) < 2) dat$d$b <- ""
    
    if ("forceDate" %in% names(opts)) optsx$type <- "date"
    
    # Create Observable chart
    chrt <- robservable(
        "@joe-heywood-gla/gla-dpa-chart",
        include = chart_opts,
        input = list(unempl = dat$d, chartopts = optsx, metaopts = mopts,
                     inpopts = list())
    )
    
    list(d = dat$d, co = optsx, m = mopts, chart = chrt, u = dat$u)
}

#' Order data by x-axis values.
#'
#' @param df The data frame.
#' @param xfact Factor levels for the xd column.
#'
#' @return The ordered data frame.
orderxd <- function(df, xfact) {
    if (is.null(xfact)) return(df) # Simplified return
    df[order(factor(df$xd, levels = xfact)), ] # More concise ordering
}

#' Order data by category values.
#'
#' @param df The data frame.
#' @param ctgfact Factor levels for the b column.
#'
#' @return The ordered data frame.
orderctg <- function(df, ctgfact) {
    if (is.null(ctgfact)) return(df)
    df[order(factor(df$b, levels = ctgfact)), ]
}


