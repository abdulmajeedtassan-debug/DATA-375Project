\name{artist_summary}
\alias{artist_summary}
\title{Summarize one artist's Spotify songs}
\usage{artist_summary(data, name)}
\arguments{
\item{data}{A cleaned Spotify data frame from \code{clean_music_data()}.}
\item{name}{A character string with the artist name to search for.}
}
\value{A one-row data frame with total tracks, total streams, average energy, average danceability, average bpm, and top song.}
\description{This function gives a short summary for one artist, including total songs, total streams, average audio features, and the artist's top streamed song in the data.}
\examples{\dontrun{
spotify_sample <- load_spotify_sample()
artist_summary(spotify_sample, "Taylor Swift")
}}
