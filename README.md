# musicDataAnalysis

This package analyzes the Top Spotify Songs 2023 dataset. It includes functions for cleaning the data, summarizing streams and audio features, and creating basic visualizations.

## Main functions

- `clean_music_data()` cleans the raw Spotify dataset.
- `summarize_music()` summarizes average streams and feature correlations.
- `summarize_features()` gives basic statistics for energy, danceability, and bpm.
- `top_artists()` finds the most streamed artists.
- `get_artist_features()` filters songs for one artist.
- `artist_summary()` summarizes one artist.
- `plot_music()` creates a set of common plots.
- `plot_feature_distribution()` plots one numerical feature.
- `load_spotify_sample()` loads the cleaned sample data included in the package.

## Example

```r
library(musicDataAnalysis)

spotify_sample <- load_spotify_sample()
summary <- summarize_music(spotify_sample)
plots <- plot_music(spotify_sample)
plots$scatter_energy_streams
```
