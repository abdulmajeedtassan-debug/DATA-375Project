\name{summarize_features}
\alias{summarize_features}
\title{Summarize Spotify audio features}
\usage{summarize_features(data)}
\arguments{\item{data}{A cleaned Spotify data frame from \code{clean_music_data()}.}}
\value{A one-row data frame with summary statistics for energy, danceability, and bpm.}
\description{This function calculates mean, standard deviation, minimum, maximum, and median for the main numerical audio features in the cleaned Spotify data.}
\examples{\dontrun{
spotify_sample <- load_spotify_sample()
summarize_features(spotify_sample)
}}
