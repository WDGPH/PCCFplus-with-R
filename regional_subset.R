# Processing of PCCF+ .csv files to create a simplified and efficient look up
# for a specific list of dissemination areas of residential postal codes
# Outputs 'data/PCCF+ 7E.csv'
# 
# Based on information contained in:
# Postal Code Conversion File Plus (PCCF+) Version 7E, Reference Guide

# Required libraries 
library(readr)
library(dplyr)
library(stringr)
library(magrittr)

# Parameters
DA_list = 35220000L:35239999L # list of DAs for region of interest

# Functions
# Used for WC6 through WC3
normalize_postal_wts = function(x){
  x %>% 
    group_by(`Postal Code`) %>%
    mutate(`TotalWT` = sum(`WT`)) %>%
    ungroup %>%
    mutate(`WT` = WT/TotalWT) %>%
    select(`Postal Code`, `DA`, `WT`) %>%
    arrange(`Postal Code`, `WT`) %>%
    return
}

# WC6
# 6 Digit postal codes with specified WTs
# Ensure that these WTs sum to 1
wc6 = read_csv(
  file = 'data/csv/wc6dups.csv',
  col_types = cols_only(
    PCODE   = 'c',
    PR      = 'i',
    CD      = 'i',
    DA      = 'i',
    PC6DAWT = 'd'),
  show_col_types = F) %>%
  mutate(DA = (PR * 1000000L) + (CD * 10000L) + DA) %>%
  select(
    `Postal Code` = PCODE,
    `DA`,
    `WT` = PC6DAWT) %>%
  normalize_postal_wts

# UNIQ
# Postal code - DA pairs with WT of 1
uniq = read_csv(
  file = 'data/csv/uniq.csv',
  col_types  = cols_only(
    PCODE = 'c',
    DAuid = 'i'),
  show_col_types = F) %>%
  filter(!is.na(`DAuid`)) %>% 
  mutate(`WT` = 1.0) %>%
  select(
    `Postal Code` = PCODE,
    `DA` = DAuid,
    `WT`) %>%
  arrange(`Postal Code`)

# DUPS
# Postal codes with WT split equally to set of DAs
dups = read_csv(
  file = 'data/csv/dups.csv',
  col_types  = cols_only(
    PCODE = 'c',
    DAuid = 'i'),
  show_col_types = F) %>%
  filter(!is.na(DAuid)) %>%
  select(`Postal Code` = PCODE, DA = DAuid) %>%
  group_by(`Postal Code`, DA) %>%
  summarize(combocount = n(), .groups = "drop") %>%
  group_by(`Postal Code`) %>%
  mutate(totalcount = sum(combocount)) %>%
  ungroup %>%
  mutate(`WT` = combocount/totalcount) %>%
  select(
    `Postal Code`,
    `DA`,
    `WT`) %>%
  arrange(`Postal Code`, `WT`)

# Find unique postal codes in each file
wc6_pc  = wc6  %>% use_series(`Postal Code`) %>% unique
uniq_pc = uniq %>% use_series(`Postal Code`) %>% unique
dups_pc = dups %>% use_series(`Postal Code`) %>% unique

# Some postal may codes appear in multiple files.
# Ensure we properly follow assignment hierarchy.
# Eliminate postal codes if already mapped in a file
# that should be applied earlier.

# Which unique PCs are not in wc6?
uniq_pc %<>% subset(!(uniq_pc %in% wc6_pc))

# Which dup PCs are not in uniq OR wc6?
dups_pc %<>% subset(!(dups_pc %in% uniq_pc | dups_pc %in% wc6_pc))

uniq %<>% filter(`Postal Code` %in% uniq_pc)
dups %<>% filter(`Postal Code` %in% dups_pc)

# Continue to 5, 4, and 3 Character Postal Codes
# Ensure that the WTs sum to 1 for each
wc5 = read_csv(
  file = 'data/csv/wc5dups.csv',
  col_types = cols_only(
    PCODE5  = 'c',
    PR      = 'i',
    CD      = 'i',
    DA      = 'i',
    PC5DAWT = 'd'),
  show_col_types = F) %>%
  mutate(DA = (PR * 1000000L) + (CD * 10000L) + DA) %>%
  select(
    `Postal Code` = PCODE5,
    `DA`,
    `WT` = PC5DAWT) %>%
  normalize_postal_wts

wc4 = read_csv(
  file = 'data/csv/wc4dups.csv',
  col_types = cols_only(
    PCODE4  = 'c',
    PR      = 'i',
    CD      = 'i',
    DA      = 'i',
    PC4DAWT = 'd'),
  show_col_types = F) %>%
  mutate(DA = (PR * 1000000L) + (CD * 10000L) + DA) %>%
  select(
    `Postal Code` = PCODE4,
    `DA`,
    `WT` = PC4DAWT) %>%
  normalize_postal_wts

wc3 = read_csv(
  file = 'data/csv/wc3dups.csv',
  col_types  = cols_only(
    PCODE3  = 'c',
    PR      = 'i',
    CD      = 'i',
    DA      = 'i',
    PC3DAWT = 'd'),
  show_col_types = F) %>%
  mutate(DA = (PR * 1000000L) + (CD * 10000L) + DA) %>%
  select(
    `Postal Code` = PCODE3,
    `DA`,
    `WT` = PC3DAWT) %>%
  normalize_postal_wts

# Combine files
pccf1 = bind_rows(list(wc6, uniq, dups, wc5, wc4, wc3))

# Test: Postal codes should have a total weight not greater than 1
pccf1 %>%
  group_by(`Postal Code`) %>%
  summarize(`Total Probability` = sum(WT), .groups = 'drop') %>%
  filter(`Total Probability` >= 1.00001) %>%
  nrow %>%
  cat(" postal codes with a total weight exceeding 1 (should be 0)\n")

# Test: Postal codes should have a total weight not less than 1
pccf1 %>%
  group_by(`Postal Code`) %>%
  summarize(`Total Probability` = sum(WT), .groups = 'drop') %>%
  filter(`Total Probability` <= 0.99999) %>%
  nrow %>%
  cat(" postal codes with a total weight less than 1 (should be 0)\n")

# Simplify for region
# Drop non-Ontario
# Denote all out of region as 35000000
pccf2 = pccf1 %>%
  filter(DA %/% 1000000L == 35) %>%
  mutate(DA = if_else(DA %in% DA_list, DA, 35000000L)) %>%
  group_by(`Postal Code`, `DA`) %>%
  summarize(WT = sum(WT), .groups = 'drop')

# Determine FSAs having any WT within region of interest
# Only these FSAs should be included in output
# Create a regex pattern to match these FSAs
FSAs = pccf2 %>%
  filter(DA != 35000000L) %>%
  use_series(`Postal Code`) %>%
  str_extract("^\\w\\d\\w") %>%
  unique %>%
  paste0("^", ., collapse = "|")

# Final simplification, only Postal Codes with a relevant FSA
pccf3 = pccf2 %>%
  filter(str_detect(`Postal Code`, pattern = FSAs))

# Write out final table as an CSV file
write_csv(pccf3, 'data/PCCF+ 7E regional subset.csv')

# Clean up
rm(pccf1, pccf2, pccf3,
   uniq, dups,
   wc6, wc5, wc4, wc3,
   uniq_pc, dups_pc, wc6_pc,
   DA_list, FSAs, normalize_postal_wts)