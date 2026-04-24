\name{get_artist_features}
\alias{get_artist_features}
\title{Get all songs and features for one artist}
\usage{get_artist_features(data, artist_name)}
\arguments{
\item{data}{A cleaned Spotify data frame from \code{clean_music_data()}.}
\item{artist_name}{A character string with the artist name to search for.}
}
\value{A filtered data frame containing songs by the selected artist.}
\description{This function filters the cleaned Spotify data to show songs for one artist. The search ignores capitalization, so names can be typed in upper or lower case.}
\examples{\dontrun{
spotify_sample <- load_spotify_sample()
get_artist_features(spotify_sample, "Taylor Swift")
}}
