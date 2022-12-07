#' Print "fars_read"
#'
#' This function extracts a database from a csv file and turns it into a table,
#' ignoring all simple diagonistic messages and not showing any progress bar. It
#' returns error if the file does not exist.
#'
#' @param filename A character string for the filename with a csv format that
#'                 contains a database that will be extracted to an object called "data"
#'
#' @return This function returns a data frame containing the data in the table
#'
#' @importFrom This function uses functions from other packages i.e
#'             tbl_df() from dplyr, and read_csv from readr.
#'
#' @example fars_read(accident_2013.csv.bz2)

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Print "make_filename"
#'
#' This function is to take a year as an input Then create and print a name of
#' a file with format "accident_X.csv.bz2" where X is the input year.
#' It will cause error if the file of the input year does not exist
#'
#'
#' @param year year a character string specifying the accident year.
#'
#' @return this function returns a character vector of file name "accident_X.csv.bz2"
#'         where X will be replaced by the year
#'
#' @example make_filename(2013)

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Print "fars_read_years"
#'
#' The function is to take a list of years and returns a list of data frames
#' with MONTH and year columns based on data in "accident_X.csv.bz2" files.
#' The files need to be in the working directory (Check function fars_read above!).
#'
#' @param years A list of years of integer or string that when the files were generated
#'
#' @return this function returns a list of data frames with MONTH and year columns
#'
#' @example fars_read_years(2013:2015)

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

#' print "fars_summarize_years"
#'
#' This function create a summary for some years from the input files.
#' The function uses fars_read_years() to collect the data.
#' The summarized data is a non-tidy table of the number of accidents for a give month and year.
#'
#' @param years List or vector of the years to be extracted from the datafile.
#'
#' @return This function returns a data frame. The data frame has 12 rows, each month has a row
#'         The data frame has one column for the month plus each year from the years argument has its own column.
#'         specific element in a row for a given year contains the number of accidents from the sample data.
#'
#' @example fars_summarize_years(years=c(2014,2015))

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' print "fars_map_state"
#'
#' Draws a map of the specified state showing the reported accidents coordinates for the given year.
#' Throws an error if the state number isn't valid or if there are no accidents to plot.
#' Also doesn't work if provided parameters are vectors
#'
#'  @param year An integer or string indicating the year.
#'
#'  @param state.num An integer that represents the state number.
#'
#'  @return Plots a map of the state, where the fatalities are the dots on the map.
#'
#'  @example fars_map_state(30, 2013)

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
