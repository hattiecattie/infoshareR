
#' Convert infoshare dates to YYYY-MM format
#'
#' Infoshare files often contain dates ending in 'MXX' or 'QX'. This function converts these dates into YYYY-MM format.
#' Note: the output is not in a true date format as the underlying data may not refer to a specific date, but is easier to convert into dates as desired.
#'
#' @param Date The name of the column or object containing Infoshare-formatted dates
#'
#' @return A character vector with dates in YYYY-MM format
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' data(electronic_card_transactions)
#' electronic_card_transactionst$Dates <- change_infoshare_dates(electronic_card_transactions$Dates)
#' }
change_infoshare_dates <- function(Date) {
  case_when(
    endsWith(Date, "Q1") ~ paste0(substr(Date, 1, 4), "-", "03"),
    endsWith(Date, "Q2") ~ paste0(substr(Date, 1, 4), "-", "06"),
    endsWith(Date, "Q3") ~ paste0(substr(Date, 1, 4), "-", "09"),
    endsWith(Date, "Q4") ~ paste0(substr(Date, 1, 4), "-", "12"),
    substr(Date, 5, 5) == "M" ~ paste0(substr(Date, 1, 4), "-", substr(Date, 6, 7)),
    TRUE ~ Date

  )

}


#' Remove Infoshare footnotes
#'
#' @param df A dataframe, or an object coercible to a dataframe, with annoying Infoshare footnotes you would like to remove
#'
#' @return A dataframe without the Infoshare footnotes.
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' data(electronic_card_transactions)
#' tail(electronic_card_transactions)
#' electronic_card_transactions <- remove_infoshare_footnotes(electronic_card_transactions)
#' }
remove_infoshare_footnotes <- function(df) {

  blank_if_na <- function(x) {x <- replace_na(x, ""); return(x)}

  if (!is.data.frame(df))
    warning(paste("Converting to data.frame from", class(df)))


  df <- as.data.frame(df) %>%
    mutate(across(everything(), blank_if_na)) %>%
    slice(-(grep("Table information", df[, 1]):nrow(df)))

  df
}


#' Tidy up data from Infoshare into a usable dataframe
#'
#' @param df Required: a data frame, or object coercible to data frame, downloaded from infoshare using its CSV download function. The table downloaded must be in the default format provided by Infoshare and must not contain flags.
#' @param dimension_names A character vector containing the desired names of dimensions (qualitative / categorical variables).
#' @param measure_names A character vector containing the desired names of measures (quantitative / numerical variables). If unspecified, measure names will be used as they appear in the original table. If no measure names appear in the original table,
#' @param old_measure_names A character vector of measure names, as they appear in the original table. Used only to ensure measure_names replace old_measure_names in the appropriate order.
#' @param coerce_to_numeric Logical: coerce data values to numeric? Converts all suppressed or unavailable cells to NA. Defaults to TRUE.
#' @param handle_dates Logical: call handle_infoshare_dates? Defaults to FALSE.
#' @param long Logical: if TRUE, measure names will appear in a column named Measure and values will appear in a column named Value. If FALSE, each measure will appear in an individual column. Defaults to TRUE.
#' @param measures_in_row A single numeric value giving the table row in which measure names appear. Defaults to the row after the named dimensions, if there is a row containing measure names at all.
#'
#' @return A data frame with a column named Date, a column for each dimension, and either columns Measure and Value or one column per measure.
#' @import tidyr
#' @import dplyr
#' @import tibble
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' data(regional_gdp_by_industry)
#' head(regional_gdp_by_industry)
#' wrangle_infoshare(
#'                   regional_gdp_by_industry,
#'                   dimension_names = c("Region", "Industry"),
#'                   measure_names = "GDP",
#'                   measures_in_row = 1,
#'                   long = FALSE
#'                   )
#' }
#'
wrangle_infoshare <- function(df,
                              dimension_names,
                              measure_names = NA,
                              old_measure_names = NA,
                              coerce_to_numeric = TRUE,
                              handle_dates = FALSE,
                              long = TRUE,
                              measures_in_row = NA) {
  df <- remove_infoshare_footnotes(df)

  # deals with inconsistencies in file reading
  if (!(df[1, 1] %in% c("", " ", NA)))
    df <- df %>% slice(-1)

  n_dim <- length(dimension_names)

  # checks whether the row after dimensions contains measure names
  has_measures <- df[n_dim + 1, 1] %in% c("", " ", NA)


  # reorders to put measures after dimensions (indexes from first row with actual values)
  if (has_measures & !is.na(measures_in_row)) {
    df <-
      df %>% slice((1:(n_dim + 1))[-measures_in_row], measures_in_row, (n_dim+2):nrow(df))

  }

  # extracts existing measure names if present, otherwise uses Value as a generic name
  if (has_measures & is.na(old_measure_names)) {
    old_measure_names <- df %>% slice(n_dim + 1) %>% unlist() %>% unique()
    old_measure_names <- old_measure_names[!(old_measure_names %in% c("", " ", NA))]
  } else if (is.na(old_measure_names)) {
    old_measure_names <- "Value"
  }

  # if no new measure names specified, uses existing measure names
  if (length(measure_names) < 2 & is.na(measure_names))
    measure_names <- old_measure_names

  # used to determine df shape
  n_meas <- length(measure_names)

  # fills in blank cells for dimension and measure values
  na_if_blank <- function(x) {x <- na_if(x, ""); return(x)}

  df <-
    as.data.frame(t(df)) %>%
    mutate(across(1:(n_dim + has_measures), na_if_blank)) %>%
    fill(1:(n_dim + has_measures))

  rownames(df) <- NULL


  # easiest case with a single measure - names columns and moves dates to their own column
  if (n_meas == 1) {

    if(!is.na(measures_in_row)) {
      df <- df[, -(n_dim + 1)]
    }

    colnames(df) <- c(dimension_names, df[1, (n_dim + 1):ncol(df)])


    if (!long) {
      df <- df %>% slice(-1) %>%
        pivot_longer(
          cols = (n_dim + 1):ncol(df),
          names_to = "Date",
          values_to = measure_names
        )
    } else {
      df <- df %>% slice(-1) %>%
        pivot_longer(
          cols = (n_dim + 1):ncol(df),
          names_to = "Date",
          values_to = "Value"
        )

      df <- df %>%
        add_column("Measure" = rep(measure_names, length.out = nrow(df)),
                   .before = "Value")
    }


    # for multiple measures - names columns, relabels measures, pivots dates out
  } else {
    colnames(df) <-
      c(dimension_names, "Measure", df[1, (n_dim + 2):ncol(df)])

    # rename measures
    level_key <-
      as.data.frame(cbind("Measure" = old_measure_names, measure_names))

    df <-
      left_join(df, level_key, by = "Measure") %>% select(.data$Measure) %>%
      rename("Measure" = measure_names)


    df <-
      df %>% slice(-1) %>%
      select(all_of(dimension_names), .data$Measure, everything()) %>%
      pivot_longer(
        cols = (n_dim + 2):ncol(df),
        names_to = "Date",
        values_to = "Value"
      )

    if (!long) {
      df <- df %>% pivot_wider(names_from = "Measure",
                               values_from = "Value")
    } else {
      df
    }
  }


  # deals with suppression making columns non-numeric
  if (coerce_to_numeric) {
    if (long)
      df <- df %>% mutate(Value = as.numeric(.data$Value))
    if (!long)
      df <- df %>% mutate(across(measure_names, as.numeric))
  }

  # make gross MXX and QX dates better
  if (handle_dates) {
    df <- df %>% mutate(Date = change_infoshare_dates(.data$Date))

  }

  # put the date at the start like a good human
  df <- df %>% select("Date", everything())

  return(df)

}
