\name{load_spotify_sample}
\alias{load_spotify_sample}
\title{Load the included cleaned Spotify sample data}
\usage{load_spotify_sample()}
\value{A cleaned Spotify data frame with track, artist, streams, energy, danceability, and bpm columns.}
\description{This function loads the cleaned Spotify CSV file included with the package. It is useful for testing the package functions without downloading the raw dataset again.}
\examples{\dontrun{
spotify_sample <- load_spotify_sample()
head(spotify_sample)
}}
