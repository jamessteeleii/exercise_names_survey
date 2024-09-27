# _targets.R file
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)
source("R/functions.R")
tar_option_set(
  packages = c(
    "here",
    "fastDummies",
    "ggstats",
    "gtsummary",
    "tidytext",
    "hunspell",
    "tidyverse",
    "base",
    "stringi",
    "scales",
    "wordcloud",
    "patchwork",
    "marginaleffects",
    "forcats",
    "igraph",
    "ggraph",
    "tidygraph",
    "grateful"
  )
)

list(
  # Read in and prepare data
  tar_target(key_file, here("data","Key.csv"), format = "file"),
  tar_target(key, read_key(key_file)),
  tar_target(data, read_prepare_data(key)), # files called directly in function
  tar_target(tokens, convert_to_tokens(data)),
  tar_target(bigrams, convert_to_bigrams(data)),

  # Analyses and plots
  tar_target(demographics_tbl, make_demographics_tbl(data)),
  tar_target(counts_freqs_plot, plot_counts_freqs(tokens)),
  tar_target(freqs_recognise_plot, plot_freqs_recognise(tokens)),
  tar_target(freqs_exercise_plot, plot_freqs_exercise(tokens)),
  tar_target(
    freqs_exercise_recognise_plot,
    plot_freqs_exercise_recognise(tokens)
  ),
  tar_target(tf_ief_plot, plot_tf_ief(tokens)),
  tar_target(tf_ibpf_plot, plot_tf_ibpf(tokens)),
  tar_target(tf_ibpartf_plot, plot_tf_ibpartf(tokens)),
  tar_target(tf_iaf_plot, plot_tf_iaf(tokens)),
  tar_target(tf_iadf_plot, plot_tf_iadf(tokens)),
  tar_target(tf_iequipf_plot, plot_tf_iequipf(tokens)),
  tar_target(tf_ibuf_plot, plot_tf_ibuf(tokens)),
  tar_target(recognise_bigrams_plot, plot_recognise_bigrams(bigrams)),
  tar_target(did_not_recognise_bigrams_plot, plot_did_not_recognise_bigrams(bigrams)),
  tar_target(exercise_names_likert_plot, plot_likert_exercise_names(data)),

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
    tf_ibpf_plot_tiff,
    ggsave(
      tf_ibpf_plot,
      filename = "plots/tf_ibpf_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 10,
      h = 5
    )
  ),

  tar_target(
    tf_ibpartf_plot_tiff,
    ggsave(
      tf_ibpartf_plot,
      filename = "plots/tf_ibpartf_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 10,
      h = 10
    )
  ),

  tar_target(
    tf_iaf_plot_tiff,
    ggsave(
      tf_iaf_plot,
      filename = "plots/tf_iaf_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 10,
      h = 10
    )
  ),

  tar_target(
    tf_iadf_plot_tiff,
    ggsave(
      tf_iadf_plot,
      filename = "plots/tf_iadf_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 10,
      h = 5
    )
  ),

  tar_target(
    tf_iequipf_plot_tiff,
    ggsave(
      tf_iequipf_plot,
      filename = "plots/tf_iequipf_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 10,
      h = 5
    )
  ),

  tar_target(
    tf_ibuf_plot_tiff,
    ggsave(
      tf_ibuf_plot,
      filename = "plots/tf_ibuf_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 12.5,
      h = 5
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
  ),

  tar_target(
    exercise_names_likert_plot_tiff,
    ggsave(
      exercise_names_likert_plot,
      filename = "plots/exercise_names_likert_plot.tiff",
      device = "tiff",
      dpi = 300,
      w = 7.5,
      h = 5
    )
  ),

  # Reporting
  tar_target(grateful_report, cite_packages(out.dir = ".", cite.tidyverse = TRUE, out.format = "pdf"))




)
