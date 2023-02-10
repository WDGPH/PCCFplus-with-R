# Produces code to convert PCCF+ txt files to csv
#
# Based on information contained in:
# Postal Code Conversion File Plus (PCCF+) Version 7E, Reference Guide

# Required libraries 
library(dplyr)
library(stringr)
library(magrittr)
library(tidyr)
library(purrr)

# Creates the data dictionary (list of tables)
source("meta/data_dictionary.R")

# Function for generating data extraction code using data dictionary
source("meta/code_gen.R")

# Generate "PCCF_txt_to_csv.R"
# Header and common code
cat(
  "# Code to convert PCCF+ txt files to csv",
  "#",
  "# Based on information contained in:",
  "# Postal Code Conversion File Plus (PCCF+) Version 7E, Reference Guide",
  "",
  "# Required libraries ",
  "library(readr)",
  "library(dplyr)",
  "library(stringr)",
  "library(magrittr)",
  "",
  "# functions to assist in string indexes to target type",
  "extract_dte = function(x, i, j) {",
  "  str_sub(x, i, j) %>%",
  "    str_squish %>%",
  "    as.Date(format = '%Y%m%d')",
  "}",
  "",
  "extract_txt = function(x, i, j) {",
  "  str_sub(x, i, j) %>%",
  "    str_squish %>%",
  "    na_if('')",
  "}",
  "",
  "extract_num = function(x, i, j) {",
  "  str_sub(x, i, j) %>%",
  "    str_squish %>%",
  "    as.numeric",
  "}",
  "",
  "",
  sep = "\n", file = "PCCF_txt_to_csv.R", append = F)

# File-specific generated code
iwalk(data_dictionary, extraction_code_gen, output = "PCCF_txt_to_csv.R", append = T)

# Clean up
cat(
  "# Clean up",
  "rm(extract_txt, extract_num)",
  sep = "\n", file = "PCCF_txt_to_csv.R", append = T)


# Clean up
rm(extraction_code_gen)