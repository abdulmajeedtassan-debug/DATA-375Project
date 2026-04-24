\name{plot_music}
\alias{plot_music}
\title{Create standard Spotify music plots}
\usage{plot_music(data, n = 10)}
\arguments{
\item{data}{A cleaned Spotify data frame from \code{clean_music_data()}.}
\item{n}{The number of artists to show in the bar chart. The default is 10.}
}
\value{A named list of ggplot objects: \code{scatter_energy_streams}, \code{histogram_energy}, \code{histogram_danceability}, \code{histogram_bpm}, and \code{bar_top_artists}.}
\description{This function creates a set of common ggplot2 figures for the cleaned Spotify data, including a scatterplot, histograms, and a bar chart of top artists.}
\examples{\dontrun{
spotify_sample <- load_spotify_sample()
plots <- plot_music(spotify_sample)
plots$scatter_energy_streams
}}
