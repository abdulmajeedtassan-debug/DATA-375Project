\name{summarize_music}
\alias{summarize_music}
\title{Summarize Spotify music popularity and feature relationships}
\usage{summarize_music(data)}
\arguments{\item{data}{A cleaned Spotify data frame from \code{clean_music_data()}.}}
\value{A one-row data frame with average streams and correlations.}
\description{This function calculates average streams and the correlation between streams and the main audio features: energy, danceability, and bpm.}
\examples{\dontrun{
spotify_sample <- load_spotify_sample()
summarize_music(spotify_sample)
}}
