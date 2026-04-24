\name{clean_music_data}
\alias{clean_music_data}
\title{Clean Spotify music data}
\usage{clean_music_data(data)}
\arguments{\item{data}{A raw Spotify data frame. It should include \code{track_name}, \code{artist(s)_name}, \code{streams}, \code{energy_\%}, \code{danceability_\%}, and \code{bpm}.}}
\value{A cleaned data frame with six columns: \code{track_name}, \code{artist_name}, \code{streams}, \code{energy}, \code{danceability}, and \code{bpm}.}
\description{This function cleans the raw Spotify dataset by selecting the columns needed for the project, renaming them to simpler names, converting streams to a numeric column, and removing missing values.}
\examples{\dontrun{
spotify_data <- read.csv("spotify-2023.csv", check.names = FALSE)
cleaned <- clean_music_data(spotify_data)
head(cleaned)
}}
