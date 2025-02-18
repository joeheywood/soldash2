# tests/test-db_interface.R
library(testthat)
library(RSQLite)
library(glue)
library(readr)  # If you're using readr in your functions

# Create a temporary in-memory database for testing
test_db <- tempfile(fileext = ".sqlite")
file.copy("data/sol_v4.db", test_db) # Copy your existing db to the test location

# Load the functions you want to test
source("R/db_interface.R")  # Adjust path if needed

test_that("db_connect and db_disconnect work", {
    conn <- dbConnect(SQLite(), test_db)
    expect_s4_class(conn, "SQLiteConnection") # Check if it's a connection object
    dbDisconnect(conn) # You'll need to add db_disconnect to your db_interface.R
    expect_error(dbGetQuery(conn, "SELECT 1"), "Invalid or closed connection") # Check if disconnected
})


test_that("get_charts_list returns correct data", {
    charts <- get_charts_list()
    expect_type(charts, "character")
    expect_named(charts)  # Check if it has names (chart titles)
    expect_gt(length(charts), 0) # Check if it's not empty
})

test_that("get_chapters returns correct data", {
    chapters <- get_chapters()
    expect_s3_class(chapters, "data.frame")
    expect_equal(ncol(chapters), 1)
    expect_equal(colnames(chapters), "sol_chapter")
    expect_gt(nrow(chapters), 0)
})

test_that("get_subsections returns correct data", {
    # Assuming you have at least one chapter in your test database
    some_chapter <- get_chapters()$sol_chapter[1]  # Get the first chapter
    subsections <- get_subsections(some_chapter)
    expect_s3_class(subsections, "data.frame")
    expect_equal(ncol(subsections), 1)
    expect_equal(colnames(subsections), "sol_subsection")
    # You might want to add a more specific expectation based on your data.
})

test_that("get_charts returns correct data", {
    # Assuming you have at least one chapter and subsection
    some_chapter <- get_chapters()$sol_chapter[1]
    some_subsection <- get_subsections(some_chapter)$sol_subsection[1]
    charts <- get_charts(some_chapter, some_subsection)
    expect_type(charts, "character")
    expect_named(charts)
    # Again, add more specific expectations if possible.
})

# test_that("refresh_meta updates the table", {
#     # You might want to create a small test meta.csv file
#     # and compare the database content before and after refresh_meta()
#     # This test depends on your data setup.
#     expect_silent(refresh_meta())  # Check if it runs without errors
# })
# 
# test_that("refresh_analysis updates the table", {
#     # Similar to refresh_meta, this will depend on your data setup.
#     expect_silent(refresh_analysis())
# })
# 
# test_that("get_html_for_chart returns correct data", {
#     # Assuming you have data in your analysis table
#     some_dataset <- get_charts_list()[1] # Get a dataset name
#     html <- get_html_for_chart(some_dataset)
#     expect_type(html, "character")
#     # You can add more specific expectations about the HTML content.
# })

# Clean up the temporary database after the tests
# Comment this out if you want to inspect the test database after the test run
# file.remove(test_db)