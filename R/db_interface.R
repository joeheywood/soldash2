library(RSQLite)
library(glue)
library(readr)
library(gluedown)
library(purrr)
library(assertthat)

fl <- "data/sol_v4.db"

#' Get a list of available charts.
#'
#' Retrieves a list of dataset names and their corresponding chart titles from the
#' `mtd` table in the SQLite database.
#'
#' @return A named character vector where the names are chart titles and the
#'   values are dataset names. Returns an empty character vector if an error occurs.
#'
#' @examples
#' charts <- get_charts_list()
#' names(charts) # Chart titles
#' charts        # Dataset names
get_charts_list <- function() {
    tryCatch({
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn)) # Ensure connection is closed even on error
        dat <- dbGetQuery(cn, "SELECT dataset, charttitle FROM mtd")
        out <- dat$dataset
        names(out) <- dat$charttitle
        return(out)
    }, error = function(e) {
        print(paste("Error getting chart list:", e$message))
        character() # Return empty vector on error
    })
}

#' Get distinct chapter names.
#'
#' Retrieves a list of distinct `sol_chapter` values from the `mtd` table.
#'
#' @return A data frame containing distinct chapter names in the `sol_chapter` column.
#'   Returns an empty data frame if an error occurs.
#'
#' @examples
#' chapters <- get_chapters()
get_chapters <- function() {
    tryCatch({
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn))
        return(dbGetQuery(cn, "SELECT DISTINCT sol_chapter FROM mtd"))
    }, error = function(e) {
        print(paste("Error getting chapters:", e$message))
        data.frame(sol_chapter = character()) # Return empty data frame
    })
}

#' Get distinct subsection names for a given chapter.
#'
#' Retrieves a list of distinct `sol_subsection` values for a specific
#' `sol_chapter` from the `mtd` table.
#'
#' @param chpt The chapter name.
#'
#' @return A data frame containing distinct subsection names in the `sol_subsection` column.
#'   Returns an empty data frame if an error occurs.
#'
#' @examples
#' subsections <- get_subsections("Introduction")
get_subsections <- function(chpt) {
    tryCatch({
        print(chpt)
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn))
        return(dbGetQuery(cn, glue("SELECT DISTINCT sol_subsection FROM mtd WHERE sol_chapter = '{chpt}'")))
    }, error = function(e) {
        print(paste("Error getting subsections:", e$message))
        data.frame(sol_subsection = character()) # Return empty data frame
    })
}

#' Get charts for a given chapter and subsection.
#'
#' Retrieves a list of dataset names and their corresponding chart titles for a
#' specific `sol_chapter` and `sol_subsection` from the `mtd` table.
#'
#' @param chpt The chapter name.
#' @param sub The subsection name.
#'
#' @return A named character vector where the names are chart titles and the
#'   values are dataset names. Returns an empty character vector if an error occurs.
#'
#' @examples
#' charts <- get_charts("Introduction", "Overview")
get_charts <- function(chpt, sub) {
    tryCatch({
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn))
        dat <- dbGetQuery(cn, glue("SELECT dataset, charttitle FROM mtd WHERE sol_chapter = '{chpt}' AND sol_subsection = '{sub}'"))
        x <- dat$dataset
        names(x) <- dat$charttitle
        return(x)
    }, error = function(e) {
        print(paste("Error getting charts:", e$message))
        character() # Return empty vector
    })
}


#' Get metadata for a given chart
#'
#' Retrieves a row from the `mtd` table for a given chart
#'
#' @param dtst The chapter name.
#'
#' @return A named character vector
#'
#' @examples
#' charts <- get_charts("Introduction", "Overview")
get_meta_for_chart <- function(dtst) {
    tryCatch({
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn))
        return(dbGetQuery(cn, glue("SELECT sol_chapter, sol_subsection FROM mtd WHERE dataset = '{dtst}'")))
    }, error = function(e) {
        print(paste("Error getting meta for chart:", e$message))
        character() # Return empty vector
    })
}


#' Append data to the ind_dat table
#'
#' @return Numeric value of number of rows added to the table
#'
append_ind_dat <- function(df) {
    tryCatch({
        dtst <- unique(df$dataset)
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn))
        if(!all(dbListFields(cn, "ind_dat)") %in% names(df))) {
            stop("JH ERROR. You're missing columns")
        }
        df[, dbListFields(cn, "ind_dat")]
        if(!length(dtst) %in% 1) {
            stop("JH ERROR. Not a unique identifier")
        }
        dbSendQuery(cn, glue("DELETE FROM ind_dat WHERE dataset = '{dtst}'"))
        dbAppendTable(cn, "ind_dat", df)
    }, error = function(e) {
        print(paste("Error refreshing metadata:", e$message))
    })
}

#' Refresh the metadata table.
#'
#' Reads metadata from a CSV file (`data/meta.csv`) and writes it to the `mtd`
#' table in the SQLite database, overwriting the existing table.
#'
#' @return None. This function is called for its side effect.
#'
#' @examples
#' refresh_meta()
refresh_meta <- function() {
    tryCatch({
        meta <- read_csv("data/meta.csv")
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn))
        dbWriteTable(cn, "mtd", meta, overwrite = TRUE)
    }, error = function(e) {
        print(paste("Error refreshing metadata:", e$message))
    })
}

#' Refresh the analysis table.
#'
#' Reads analysis data from a CSV file (`data/analysis.csv`), converts the `md`
#' column to HTML using `md_convert`, and writes it to the `analysis` table in
#' the SQLite database, overwriting the existing table.
#'
#' @return None. This function is called for its side effect.
#'
#' @examples
#' refresh_analysis()
refresh_analysis <- function() {
    tryCatch({
        analysis <- read_csv("data/analysis.csv")
        analysis$html <- map_chr(analysis$md, md_convert)
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn))
        dbWriteTable(cn, "analysis", analysis, overwrite = TRUE)
    }, error = function(e) {
        print(paste("Error refreshing analysis:", e$message))
    })
}

#' Get HTML content for a chart.
#'
#' Retrieves the HTML content associated with a specific dataset from the
#' `analysis` table.
#'
#' @param dtst The dataset name.
#'
#' @return The HTML content as a character string. Returns an empty string if
#'   the dataset is not found or an error occurs.
#'
#' @examples
#' html_content <- get_html_for_chart("my_dataset")
get_html_for_chart <- function(dtst) {
    tryCatch({
        cn <- dbConnect(SQLite(), fl)
        on.exit(dbDisconnect(cn))
        dat <- dbGetQuery(cn, glue("SELECT html FROM analysis WHERE dataset = '{dtst}'"))
        if (nrow(dat) == 1) {
            return(dat$html)
        } else {
            return("")
        }
    }, error = function(e) {
        print(paste("Error getting HTML for chart:", e$message))
        return("")
    })
}


#' Get data for the chart from the SQLite database.
#'
#' @param dtst Dataset name.
#' @param sqlite_dpth Path to the SQLite database.
#' @param chart Optional filter chart name. (look to get rid of this)
#'
#' @return A list containing data, metadata, and update information.
get_data_for_chart <- function(dtst, sqlite_dpth, chart = NULL) {
    conn <- dbConnect(SQLite(), sqlite_dpth)
    on.exit(dbDisconnect(conn)) # Ensure disconnection

    m <- tbl(conn, "mtd") %>% filter(dataset == dtst) %>% collect()
    upd <- tbl(conn, "updates") %>% filter(dataset == dtst) %>% collect()

    # Fetch data
    dat <- if (!is.null(chart) && !chart %in% "keep") {
        dbGetQuery(conn, glue("SELECT * FROM ind_dat WHERE dataset = '{dtst}' and chart = '{chart}'"))
    } else {
        dbGetQuery(conn, glue("SELECT * FROM ind_dat WHERE dataset = '{dtst}'"))
    }

    # Update information processing
    if (nrow(upd) > 0) {
        upd$timestamp <- as.POSIXct(upd$timestamp, origin = "1970-1-1")
        last_update <- if (any(upd$newvals > 0, na.rm = TRUE)) {
            max(upd$timestamp[upd$newvals > 0])
        } else {
            upd$timestamp[1]
        }
        last_check <- max(upd$timestamp)
        max_xval <- ifelse(dat$xwhich[1] == 1, max(dat$xvarchar), max(dat$xvardt))
        upds <- data.frame(dataset = dtst, last_update = last_update,
                           last_check = last_check, max_xval = max_xval)
    } else {
        upds <- data.frame()
    }

    # Set xd column and type
    if (dat$xwhich[1] == 1) {
        m$type <- "character"
        dat$xd <- dat$xvarchar
    } else {
        m$type <- "date"
        dat$xd <- dat$xvardt
    }

    # Select output columns
    outdat <- if (!is.null(chart) && chart %in% "keep") {
        dat
    } else {
        dat |> select(dataset, xd, b = yvllb, y = yval, text)
    }

    list(d = outdat, m = m, u = upds) |> convert_q_date()
}

#' Convert date if it's quarterly, rather than monthly.
#' this changes the x axis
#'
#' @param obj List containing data, metadata, and update info.
#'
#' @return The modified list.
convert_q_date <- function(obj) {
    dat <- obj$d
    opts <- obj$m

    if("xd" %in% names(dat) == TRUE &&
       all(str_detect(dat$xd, "^20\\d{2} ?(/|Q)[1-4]{1}$"))) {
        dat <- dat %>%
            mutate(xd = str_replace(xd, " ", "")) %>%
            separate(xd, c("yr", "q"), sep = "(Q|/)") %>%
            mutate(xd = as.Date(paste0(yr, "-",
                                       (as.numeric(q)*3),
                                       "-01")))
        opts$type <- "quarter"
        opts$xtick <- "Quarter ending %b %Y"
        opts$frm <- min(dat$xd) - 20
        opts$tt <- max(dat$xd) + 2
        opts$quarters <- TRUE
        datrange <- difftime(max(dat$xd), min(dat$xd), units = "weeks")
        uyr <- datrange > 140
        opts$useyear <- uyr
        obj$u$useyear <- uyr
        dat$xd <- format(dat$xd, "%Y-%m-%d")
    }
    list(d = dat, m = opts, u = obj$u)
}
