library(RSQLite)
library(glue)
library(readr)
library(gluedown)
library(purrr)

fl <- "data/sol_v4.db"

# get_chart_data <- function() {
#     cn <- dbConnect(SQLite(), fl)
#     dat <- dbGetQuery(cn, "select * from chartdat limit 5")
#     dbDisconnect(cn)
#     return(dat)
# }

get_charts_list <- function() {
    cn <- dbConnect(SQLite(), fl)
    dat <- dbGetQuery(cn, "SELECT dataset, charttitle FROM mtd")
    dbDisconnect(cn)
    out <- dat$dataset
    names(out) <- dat$charttitle
    return(out)
    
}

get_chapters <- function() {
    cn <- dbConnect(SQLite(), fl)
    dat <- dbGetQuery(cn, "SELECT DISTINCT sol_chapter FROM mtd")
    dbDisconnect(cn)
    return(dat)
}

get_subsections <- function(chpt) {
    cn <- dbConnect(SQLite(), fl)
    dat <- dbGetQuery(cn, glue("SELECT DISTINCT sol_subsection FROM mtd WHERE sol_chapter = '{chpt}'"))
    dbDisconnect(cn)
    return(dat)
}

get_charts <- function(chpt, sub) {
    cn <- dbConnect(SQLite(), fl)
    dat <- dbGetQuery(cn, glue("SELECT dataset, charttitle FROM mtd WHERE sol_chapter = '{chpt}' AND sol_subsection = '{sub}'"))
    dbDisconnect(cn)
    x <- dat$dataset
    names(x) <- dat$charttitle
    return(x)
}


refresh_meta <- function() {
    meta <- read_csv("data/meta.csv")
    cn <- dbConnect(SQLite(), fl)
    # dbSendQuery(cn, "drop table mtd")
    dbWriteTable(cn, "mtd", meta, overwrite = TRUE)
    
    dbDisconnect(cn)
    
}


refresh_analysis <- function() {
    analysis <- read_csv("data/analysis.csv")
    analysis$html <- map_chr(analysis$md, md_convert)
    cn <- dbConnect(SQLite(), fl)
    dbWriteTable(cn, "analysis", analysis, overwrite = TRUE)
    
    dbDisconnect(cn)
    
}

get_html_for_chart <- function(dtst) {
    cn <- dbConnect(SQLite(), fl)
    dat <- dbGetQuery(cn, glue("SELECT html FROM analysis WHERE dataset = '{dtst}'"))
    dbDisconnect(cn)
    if(nrow(dat) == 1) {
        return(dat$html)
    } else {
        return("")
    }
}

