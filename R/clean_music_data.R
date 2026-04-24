#' Clean Spotify music data
#'
#' This function cleans the raw Spotify data so it is ready to use.
#' It keeps the main columns, gives them easier names, changes streams to
#' numbers, and removes rows with missing values.
#'
#' @param data A raw Spotify data frame.
#'
#' @return A cleaned data frame with track name, artist name, streams,
#' energy, danceability, and bpm.
#'
#' @examples
#' spotify_data <- read.csv("spotify-2023.csv", check.names = FALSE)
#' cleaned <- clean_music_data(spotify_data)
#' head(cleaned)
#'
#' @export
clean_music_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  cleaned_data <- data[, c(
    "track_name",
    "artist(s)_name",
    "streams",
    "energy_%",
    "danceability_%",
    "bpm"
  )]

  names(cleaned_data) <- c(
    "track_name",
    "artist_name",
    "streams",
    "energy",
    "danceability",
    "bpm"
  )

  cleaned_data$streams <- trimws(cleaned_data$streams)
  cleaned_data$streams <- gsub(",", "", cleaned_data$streams)
  cleaned_data$streams <- suppressWarnings(as.numeric(cleaned_data$streams))

  cleaned_data <- na.omit(cleaned_data)
  row.names(cleaned_data) <- NULL

  return(cleaned_data)
}
