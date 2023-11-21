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
    # "readxl",
    "metafor",
    "brms",
    "modelr",
    "tidybayes",
    "bayesplot",
    "bayestestR",
    "rstan",
    "ggridges",
    "janitor",
    "tidyverse",
    "base",
    "furrr",
    "patchwork",
    "marginaleffects",
    "broom.mixed"
  )
)
