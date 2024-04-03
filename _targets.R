# _targets.R file
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)
source("R/functions.R")
tar_option_set(
  packages = c(
    # "here",
    # "readxl",
    # "metafor",
    # "brms",
    # "modelr",
    # "tidybayes",
    # "bayesplot",
    # "bayestestR",
    # "rstan",
    # "ggridges",
    # "janitor",
    "tidytext",
    "hunspell",
    "tidyverse",
    "base",
    "stringi",
    "scales",
    "wordcloud",
    "lspline",
    # "furrr",
    "patchwork",
    "marginaleffects",
    "forcats",
    "igraph",
    "ggraph",
    "tidygraph"
    # "broom.mixed"
  )
)

list(
  # Read in and prepare data
  tar_target(key, read_key()),
  tar_target(data, read_prepare_data(key)),
  tar_target(tokens, convert_to_tokens(data)),
  tar_target(bigrams, convert_to_bigrams(data)),

  # Analyses and plots
  tar_target(counts_freqs_plot, plot_counts_freqs(tokens)),
  tar_target(freqs_recognise_plot, plot_freqs_recognise(tokens)),
  tar_target(freqs_exercise_plot, plot_freqs_exercise(tokens)),
  tar_target(
    freqs_exercise_recognise_plot,
    plot_freqs_exercise_recognise(tokens)
  ),
  tar_target(tf_ief_plot, plot_tf_ief(tokens)),
  tar_target(recognise_bigrams_plot, plot_recognise_bigrams(bigrams)),
  tar_target(did_not_recognise_bigrams_plot, plot_did_not_recognise_bigrams(bigrams)),

  # Save plots
  tar_target(
    counts_freqs_plot_tiff,
    ggsave(
      counts_freqs_plot,
      filename = "plots/counts_freqs_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 15,
      h = 5
    )
  ),

  tar_target(
    freqs_recognise_plot_tiff,
    ggsave(
      freqs_recognise_plot,
      filename = "plots/freqs_recognise_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 15,
      h = 5
    )
  ),

  tar_target(
    freqs_exercise_plot_tiff,
    ggsave(
      freqs_exercise_plot,
      filename = "plots/freqs_exercise_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 20,
      h = 10
    )
  ),

  tar_target(
    freqs_exercise_recognise_plot_tiff,
    ggsave(
      freqs_exercise_recognise_plot,
      filename = "plots/freqs_exercise_recognise_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 20,
      h = 10
    )
  ),

  tar_target(
    tf_ief_plot_tiff,
    ggsave(
      tf_ief_plot,
      filename = "plots/tf_ief_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 10,
      h = 10
    )
  ),


  tar_target(
    recognise_bigrams_plot_tiff,
    ggsave(
      recognise_bigrams_plot,
      filename = "plots/recognise_bigrams_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 15,
      h = 12.5
    )
  ),

  tar_target(
    did_not_recognise_bigrams_plot_tiff,
    ggsave(
      did_not_recognise_bigrams_plot,
      filename = "plots/did_not_recognise_bigrams_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 15,
      h = 12.5
    )
  )



)
