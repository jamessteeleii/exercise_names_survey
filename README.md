# Names of resistance exercises: text analysis of survey responses

## Abstract
Names of resistance exercises play an important role in education and promoting exercise participation. Yet, previous research has documented that exercises are named inconsistently among exercise professionals and within exercise textbooks and journal articles. Greater standardization or the development of a taxonomy may therefore be warranted, but large-scale data on naming practices are limited. This study examined how resistance exercises are named in a large sample. A total of 1,849 individuals completed an online survey; analyses were restricted to 1,425 respondents who passed attention checks and reported English as their primary exercise-related language. Participants were predominantly male (76%), highly educated (82% held a Bachelor’s degree or higher; 42% in exercise-related fields), and experienced in resistance training (median 10 years [IQR: 5–15]). Nearly half of respondents held relevant certifications (46%) or had worked in resistance training instruction (49%). Respondents viewed images of exercises performed with free weights, traditional machines, and a connected adaptive resistance machine. Respondents indicated whether they recognised the exercise in each image and what they call the exercise. Text analyses examined word frequencies, term-frequency–inverse exercise frequency (tf-ief), and common bigrams. Most exercises were widely recognised (>90% of respondents), except those using connected adaptive machines (62.3–67.3% of respondents). Recognition influenced naming patterns: respondents who recognised exercises used more consistent, commonly shared terminology, whereas respondents who did not recognise exercises used more idiosyncratic naming. Equipment-related terms were consistently important, but other naming components (e.g., body position, body part, movement direction, unilateral/bilateral execution) varied substantially in importance. Bigrams showed that recognised exercises were typically labelled using “textbook” word pairings, while unrecognised exercises produced more variable combinations. Participants generally agreed that exercise names are important (74%), inconsistently used (63%), and influence learning (69%). Many respondents reported using multiple names for the same exercise (73%), and most respondents supported standardization of exercise names (69%). Overall, the findings suggest that exercise naming follows broad linguistic principles (e.g., Zipf’s law) but also highlight elements of a shared “folk taxonomy”. Despite this, there was substantial inconsistency in resistance exercise naming practices. However, whilst such a folk taxonomy might enable clear communication within public and applied settings, the development of standardized terminology or a formal scientific taxonomy might ensure that cross-communication and understanding across domains is more easily achieved. Alternatively, a hierarchical taxonomy and system of translation across synonyms in use already may be a valuable pursuit.

## Reproducibility
This repository contains the necessary files and code to reproduce the analyses, figures, and the manuscript. 

## Usage
To reproduce the analyses, you will need to have R (https://cran.r-project.org/) and RStudio (https://www.rstudio.com/products/rstudio/download/#download) installed on your computer.

To help with reproducibility, this project uses the `renv` R package (see https://rstudio.github.io/renv/articles/renv.html). With `renv`, the state of this R project can be easily loaded as `renv` keeps track of the required R packages (including version), and (if known) the external source from which packages were retrieved (e.g., CRAN, Github). With `renv`, packages are installed to a project specific library rather than your user or system library. The `renv` package must be installed on your machine before being able to benefit from its features. The package can be installed using the following command:

``` r
install.packages("renv")
```

Once you have `renv` installed, you can get a copy of this repository on your machine by clicking the green Code button then choose Download zip. Save to your machine and extract. After extraction, double click the `exercise_names_survey.Rproj` file in the root directory. This will automatically open RStudio. This will ensure all paths work on your system as the working directory will be set to the location of the `.Rproj` file. Upon opening, RStudio will recognize the `renv` files and you will be informed that the project library is out of sync with the lockfile. At shown in the console pane of RStudio, running `renv::restore()` will install the packages recorded in the lockfile. This could take some time depending on your machine and internet connection.

## Targets analysis pipeline

This project also uses a function based analysis pipeline using
[`targets`](https://books.ropensci.org/targets/). Instead of script based pipelines the `targets` package makes use of functions applied to targets specified within the pipeline. The targets can be viewed in the `_targets.R` file, and any user defined functions are available in `R/functions.r`.

You can view the existing targets pipeline by clicking [here](https://jamessteeleii.github.io/exercise_names_survey/targets_pipeline.html).

Useful console functions:

- `tar_edit()` opens the make file
- `tar_make()` to run targets
- `tar_visnetwork()` to view pipeline

## Software and packages used

The [`grateful`](https://pakillo.github.io/grateful/index.html) package was used to create citations to all software and packages used in the analysis. The `grateful` report can be viewed by downloading by clicking [here](https://jamessteeleii.github.io/exercise_names_survey/grateful-report.html).

## License

Shield: [![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]

This work is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
  [cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg
