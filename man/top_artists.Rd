\name{top_artists}
\alias{top_artists}
\title{Find the top streamed artists}
\usage{top_artists(data, n = 10)}
\arguments{
\item{data}{A cleaned Spotify data frame from \code{clean_music_data()}.}
\item{n}{The number of top artists to return. The default is 10.}
}
\value{A data frame with artist names and total stream counts.}
\description{This function groups the cleaned Spotify data by artist and calculates the total streams for each artist. It returns the artists with the highest total streams.}
\examples{\dontrun{
spotify_sample <- load_spotify_sample()
top_artists(spotify_sample, n = 5)
}}
