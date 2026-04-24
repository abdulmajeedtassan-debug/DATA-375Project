#' Create Spotify music plots
#'
#' This function creates simple plots for the cleaned Spotify data. It makes a
#' scatterplot, three histograms, and a bar chart for top artists.
#'
#' @param data A cleaned Spotify data frame.
#' @param n The number of artists to show in the bar chart. The default is 10.
#'
#' @return A list of ggplot objects.
#'
#' @examples
#' spotify_sample <- load_spotify_sample()
#' plots <- plot_music(spotify_sample)
#' plots$scatter_energy_streams
#'
#' @export
plot_music <- function(data, n = 10) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  scatter_plot <- ggplot2::ggplot(data, ggplot2::aes(x = energy, y = streams)) +
    ggplot2::geom_point(alpha = 0.35) +
    ggplot2::labs(
      title = "Energy vs streams",
      x = "Energy (%)",
      y = "Streams"
    ) +
    ggplot2::theme_minimal()

  hist_energy <- ggplot2::ggplot(data, ggplot2::aes(x = energy)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::labs(
      title = "Distribution of energy",
      x = "Energy (%)",
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  hist_dance <- ggplot2::ggplot(data, ggplot2::aes(x = danceability)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::labs(
      title = "Distribution of danceability",
      x = "Danceability (%)",
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  hist_bpm <- ggplot2::ggplot(data, ggplot2::aes(x = bpm)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::labs(
      title = "Distribution of BPM",
      x = "BPM",
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  artist_totals <- aggregate(streams ~ artist_name, data = data, sum)
  artist_totals <- artist_totals[order(artist_totals$streams, decreasing = TRUE), ]
  artist_totals <- head(artist_totals, n)
  names(artist_totals) <- c("artist_name", "total_streams")

  bar_plot <- ggplot2::ggplot(
    artist_totals,
    ggplot2::aes(x = reorder(artist_name, total_streams), y = total_streams)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top", n, "artists by total streams"),
      x = "Artist",
      y = "Total streams"
    ) +
    ggplot2::theme_minimal()

  plots <- list(
    scatter_energy_streams = scatter_plot,
    histogram_energy = hist_energy,
    histogram_danceability = hist_dance,
    histogram_bpm = hist_bpm,
    bar_top_artists = bar_plot
  )

  return(plots)
}

#' Plot one feature distribution
#'
#' This function makes a histogram for one numeric feature.
#'
#' @param data A cleaned Spotify data frame.
#' @param feature The name of one numeric column, such as "energy" or "bpm".
#' @param bins The number of histogram bins. The default is 30.
#'
#' @return A ggplot object.
#'
#' @examples
#' spotify_sample <- load_spotify_sample()
#' plot_feature_distribution(spotify_sample, "bpm")
#'
#' @export
plot_feature_distribution <- function(data, feature, bins = 30) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!feature %in% names(data)) {
    stop("feature must be a column in data")
  }

  if (!is.numeric(data[[feature]])) {
    stop("feature must be numeric")
  }

  plot_data <- data.frame(value = data[[feature]])

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::labs(
      title = paste("Distribution of", feature),
      x = feature,
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  return(plot)
}
