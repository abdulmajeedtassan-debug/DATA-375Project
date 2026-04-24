#' Load the included Spotify data
#'
#' This function loads the cleaned Spotify CSV file that is included in the
#' package. It lets the user test the package without downloading the data.
#'
#' @return A cleaned Spotify data frame.
#'
#' @examples
#' spotify_sample <- load_spotify_sample()
#' head(spotify_sample)
#'
#' @export
load_spotify_sample <- function() {
  file_path <- system.file("extdata", "cleaned_spotify.csv", package = "musicDataAnalysis")

  if (file_path == "") {
    stop("cleaned_spotify.csv was not found in the package")
  }

  data <- read.csv(file_path, check.names = FALSE)
  return(data)
}
