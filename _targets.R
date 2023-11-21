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
    "wordcloud"
    # "furrr",
    # "patchwork",
    # "marginaleffects",
    # "broom.mixed"
  )
)

list(
  tar_target(key, read_key()),
  tar_target(data, read_prepare_data(key)),
  tar_target(tokens, convert_to_tokens(data))
)
