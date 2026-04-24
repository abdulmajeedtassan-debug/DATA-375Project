\name{plot_feature_distribution}
\alias{plot_feature_distribution}
\title{Plot a feature distribution}
\usage{plot_feature_distribution(data, feature, bins = 30)}
\arguments{
\item{data}{A cleaned Spotify data frame from \code{clean_music_data()}.}
\item{feature}{A character string naming one numeric column, such as \code{"energy"}, \code{"danceability"}, \code{"bpm"}, or \code{"streams"}.}
\item{bins}{The number of histogram bins. The default is 30.}
}
\value{One ggplot object showing the feature distribution.}
\description{This function plots the distribution of one numeric feature using a histogram and a density curve.}
\examples{\dontrun{
spotify_sample <- load_spotify_sample()
plot_feature_distribution(spotify_sample, "bpm")
}}
