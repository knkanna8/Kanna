utils::globalVariables(c("MONTH", "STATE", "year"))
#---------------------------------------------------------------------------------
#' @title Read fars data.
#'
#' @description This function reads a .csv file and convert in to \code{tbl} data
#' frame object. The main advantage to using a tbl_df over a regular data frame is
#' the printing: tbl objects only print a few rows and all the columns that fit on
#' one screen, describing the rest of it as text.
#'
#' @param filename name of a .csv file.
#'
#' @return It returns a  \code{tbl} data frame object.
#'
#' @note If there is no such file then it will return "file <filename> does not
#' exist".
#'
#' @import dplyr readr
#'
#' @examples
#' \dontrun{
#' fars_read("/home/kanna/Desktop/COURSERA/data/accident_2013.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {

  # Access the data in the package, if available.
  filename0 <- gsub(".csv.bz2","",filename)
  if(exists(filename0)){
    return(get(filename0))
  }

  if(!exists(filename0)){
    if(!file.exists(filename))
      stop("file '", filename, "' does not exist")
    data <- suppressMessages({
      readr::read_csv(filename, progress = FALSE)
    })
    return(dplyr::tbl_df(data))
  }
}
#OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# I included the data into the package by:
#OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# accident_2013<-dplyr::tbl_df(readr::read_csv("/home/kanna/Desktop/COURSERA/
#                              Kanna/data-raw/accident_2013.csv.bz2",progress = F))
# accident_2014<-dplyr::tbl_df(readr::read_csv("/home/kanna/Desktop/COURSERA/
#                              Kanna/data-raw/accident_2014.csv.bz2",progress = F))
# accident_2015<-dplyr::tbl_df(readr::read_csv("/home/kanna/Desktop/COURSERA/
#                              Kanna/data-raw/accident_2015.csv.bz2",progress = F))
# devtools::use_data(accident_2013)
# devtools::use_data(accident_2014)
# devtools::use_data(accident_2015)
#OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
#---------------------------------------------------------------------------------
#' @title Generate fars data file name.
#'
#' @description It generates character strings (eg: "accident_2013.csv.bz2") for
#' a given year (eg: 2013).
#'
#' @param year a numeric value.
#'
#' @return It returns character strings eg: "accident_2013.csv.bz2".
#'
#' @note If the argument is not a number then it will return "accident_NA.csv.bz2"
#' and a warning.
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#---------------------------------------------------------------------------------
#' @title Extract month and year columns in the fars data.
#'
#' @description This function returns a list of data frames. Each data frame contain only
#' the month and the year columns from fars data for each year.
#'
#' @param years a vector of numbers.
#'
#' @return It returns a list of data frame for each year. The data frame contains
#' only the month and the year columns of fars data.
#'
#' @note If the argument is not a number then it will return a NULL list with
#' a warning says that the argument is invalid.
#'
#' @import dplyr
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013,2014))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#---------------------------------------------------------------------------------
#' @title Frequency table for months in fars data.
#'
#' @description This function gives a frequency table for months in each year.
#' @param years a vector of numbers.
#'
#' @return It returns a frequency table for months in each year. The first column
#' of the output is the month number and the other columns are year-wise month
#' counts in the fars data.
#'
#' @import tidyr dplyr
#'
#' @note If the year is not in the fars data then it will return an error message.
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#---------------------------------------------------------------------------------
#' @title Plot the accidents in states in the fars data.
#'
#' @description This function plots the state and the point where the accidents
#' occured in a specific year.
#'
#' @param state.num a number which represent a state.
#'
#' @param year a number which represent a year.
#'
#' @return It returns a plot of state with points where the accidents
#' occured in a specific year.
#'
#' @import maps graphics dplyr
#' @importFrom maps map
#'
#' @note It plots the accidents which occured in latitude <= 90 and
#' longitude <= 900. If the argument \code{state.num} is beyond the number of
#' states in the fars data then it will return that "invalid STATE number".
#' If there is no accident during a specific year in  aspecific state then
#' it returns "no accidents to plot".
#'
#' @examples
#'
#' \dontrun{
#' fars_map_state(1,2013)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
#---------------------------------------------------------------------------------
