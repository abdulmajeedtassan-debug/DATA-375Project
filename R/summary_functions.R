#' Find the top streamed artists
#'
#' This function adds up streams for each artist and returns the artists with
#' the most total streams.
#'
#' @param data A cleaned Spotify data frame.
#' @param n The number of artists to return. The default is 10.
#'
#' @return A data frame with artist names and total streams.
#'
#' @examples
#' spotify_sample <- load_spotify_sample()
#' top_artists(spotify_sample, n = 5)
#'
#' @export
top_artists <- function(data, n = 10) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  artist_totals <- aggregate(streams ~ artist_name, data = data, sum)
  artist_totals <- artist_totals[order(artist_totals$streams, decreasing = TRUE), ]
  names(artist_totals) <- c("artist_name", "total_streams")
  row.names(artist_totals) <- NULL

  return(head(artist_totals, n))
}

#' Summarize Spotify music data
#'
#' This function calculates average streams and the correlation between streams
#' and energy, danceability, and bpm.
#'
#' @param data A cleaned Spotify data frame.
#'
#' @return A one-row data frame with average streams and correlations.
#'
#' @examples
#' spotify_sample <- load_spotify_sample()
#' summarize_music(spotify_sample)
#'
#' @export
summarize_music <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  result <- data.frame(
    avg_streams = mean(data$streams, na.rm = TRUE),
    correlate_energy = cor(data$streams, data$energy, use = "complete.obs"),
    correlate_dance = cor(data$streams, data$danceability, use = "complete.obs"),
    correlate_bpm = cor(data$streams, data$bpm, use = "complete.obs")
  )

  return(result)
}

#' Summarize Spotify audio features
#'
#' This function gives simple statistics for energy, danceability, and bpm.
#'
#' @param data A cleaned Spotify data frame.
#'
#' @return A data frame with the mean, standard deviation, minimum, median,
#' and maximum for the audio features.
#'
#' @examples
#' spotify_sample <- load_spotify_sample()
#' summarize_features(spotify_sample)
#'
#' @export
summarize_features <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  result <- data.frame(
    feature = c("energy", "danceability", "bpm"),
    mean = c(mean(data$energy), mean(data$danceability), mean(data$bpm)),
    sd = c(sd(data$energy), sd(data$danceability), sd(data$bpm)),
    min = c(min(data$energy), min(data$danceability), min(data$bpm)),
    median = c(median(data$energy), median(data$danceability), median(data$bpm)),
    max = c(max(data$energy), max(data$danceability), max(data$bpm))
  )

  return(result)
}

#' Get songs for one artist
#'
#' This function filters the data and returns songs for one artist.
#'
#' @param data A cleaned Spotify data frame.
#' @param artist_name The artist name to search for.
#'
#' @return A data frame with songs by the selected artist.
#'
#' @examples
#' spotify_sample <- load_spotify_sample()
#' get_artist_features(spotify_sample, "Taylor Swift")
#'
#' @export
get_artist_features <- function(data, artist_name) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  artist_data <- data[grepl(artist_name, data$artist_name, ignore.case = TRUE), ]
  row.names(artist_data) <- NULL

  return(artist_data)
}

#' Summarize one artist
#'
#' This function gives a short summary for one artist.
#'
#' @param data A cleaned Spotify data frame.
#' @param name The artist name to search for.
#'
#' @return A one-row data frame with total tracks, total streams, average
#' features, and top song.
#'
#' @examples
#' spotify_sample <- load_spotify_sample()
#' artist_summary(spotify_sample, "Taylor Swift")
#'
#' @export
artist_summary <- function(data, name) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  artist_data <- data[grepl(name, data$artist_name, ignore.case = TRUE), ]

  if (nrow(artist_data) == 0) {
    stop("No songs found for this artist")
  }

  top_song_index <- which.max(artist_data$streams)

  result <- data.frame(
    total_tracks = nrow(artist_data),
    total_streams = sum(artist_data$streams, na.rm = TRUE),
    avg_energy = mean(artist_data$energy, na.rm = TRUE),
    avg_danceability = mean(artist_data$danceability, na.rm = TRUE),
    avg_bpm = mean(artist_data$bpm, na.rm = TRUE),
    top_song = artist_data$track_name[top_song_index]
  )

  return(result)
}
