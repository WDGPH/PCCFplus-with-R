# Code to convert PCCF+ txt files to csv
#
# Based on information contained in:
# Postal Code Conversion File Plus (PCCF+) Version 7E, Reference Guide

# Required libraries 
library(readr)
library(dplyr)
library(stringr)
library(magrittr)

# functions to assist in string indexes to target type
extract_dte = function(x, i, j) {
  str_sub(x, i, j) %>%
    str_squish %>%
    as.Date(format = '%Y%m%d')
}

extract_txt = function(x, i, j) {
  str_sub(x, i, j) %>%
    str_squish %>%
    na_if('')
}

extract_num = function(x, i, j) {
  str_sub(x, i, j) %>%
    str_squish %>%
    as.numeric
}


# dups
input = list.files('data/txt', pattern='\\.DUPS\\.', full.names = T)
output = 'data/csv/dups.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE                  = extract_txt(X1, 1L, 6L),
    FSA                    = extract_txt(X1, 7L, 9L),
    PR                     = extract_txt(X1, 10L, 11L),
    CDuid                  = extract_txt(X1, 12L, 15L),
    CSDuid                 = extract_txt(X1, 16L, 22L),
    CSDname                = extract_txt(X1, 23L, 92L),
    CSDtype                = extract_txt(X1, 93L, 95L),
    CCScode                = extract_txt(X1, 96L, 98L),
    SAC                    = extract_txt(X1, 99L, 101L),
    SACtype                = extract_txt(X1, 102L, 102L),
    CTname                 = extract_txt(X1, 103L, 109L),
    ER                     = extract_txt(X1, 110L, 111L),
    DPL                    = extract_txt(X1, 112L, 115L),
    FEDuid                 = extract_txt(X1, 116L, 120L),
    Pop_Cntr_RA            = extract_txt(X1, 121L, 124L),
    Pop_Cntr_RA_type       = extract_txt(X1, 125L, 125L),
    DAuid                  = extract_txt(X1, 126L, 133L),
    DB                     = extract_txt(X1, 134L, 136L),
    Rep_Pt_Type            = extract_txt(X1, 137L, 137L),
    LAT                    = extract_num(X1, 138L, 148L),
    LONG                   = extract_num(X1, 149L, 161L),
    SLI                    = extract_txt(X1, 162L, 162L),
    PCtype                 = extract_txt(X1, 163L, 163L),
    Comm_Name              = extract_txt(X1, 164L, 193L),
    DMT                    = extract_txt(X1, 194L, 194L),
    H_DMT                  = extract_txt(X1, 195L, 195L),
    DMTDIFF                = extract_txt(X1, 196L, 196L),
    Birth_Date             = extract_dte(X1, 197L, 204L),
    Ret_Date               = extract_dte(X1, 205L, 212L),
    PO                     = extract_txt(X1, 213L, 213L),
    QI                     = extract_txt(X1, 214L, 216L),
    Source                 = extract_txt(X1, 217L, 217L),
    Pop_Cntr_RA_Size_Class = extract_txt(X1, 218L, 218L),
    nCD                    = extract_num(X1, 219L, 219L),
    nCSD                   = extract_num(X1, 220L, 220L),
    nDA                    = extract_num(X1, 221L, 221L),
    nDB                    = extract_num(X1, 222L, 222L),
    keeprec                = extract_num(X1, 223L, 223L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# pointdup
input = list.files('data/txt', pattern='\\.POINTDUP\\.', full.names = T)
output = 'data/csv/pointdup.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE  = extract_txt(X1, 1L, 6L),
    nPCODE = extract_num(X1, 7L, 10L),
    ObsDup = extract_num(X1, 11L, 18L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# rpo
input = list.files('data/txt', pattern='\\.RPO\\.', full.names = T)
output = 'data/csv/rpo.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE                  = extract_txt(X1, 1L, 6L),
    FSA                    = extract_txt(X1, 7L, 9L),
    PR                     = extract_txt(X1, 10L, 11L),
    CDuid                  = extract_txt(X1, 12L, 15L),
    CSDuid                 = extract_txt(X1, 16L, 22L),
    CSDname                = extract_txt(X1, 23L, 92L),
    CSDtype                = extract_txt(X1, 93L, 95L),
    CCScode                = extract_txt(X1, 96L, 98L),
    SAC                    = extract_txt(X1, 99L, 101L),
    SACtype                = extract_txt(X1, 102L, 102L),
    CTname                 = extract_txt(X1, 103L, 109L),
    ER                     = extract_txt(X1, 110L, 111L),
    DPL                    = extract_txt(X1, 112L, 115L),
    FED                    = extract_txt(X1, 116L, 118L),
    Pop_Cntr_RA            = extract_txt(X1, 121L, 124L),
    Pop_Cntr_RA_type       = extract_txt(X1, 125L, 125L),
    DAuid                  = extract_txt(X1, 126L, 133L),
    DB                     = extract_txt(X1, 134L, 136L),
    Rep_Pt_Type            = extract_txt(X1, 137L, 137L),
    LAT                    = extract_num(X1, 138L, 148L),
    LONG                   = extract_num(X1, 149L, 161L),
    SLI                    = extract_txt(X1, 162L, 162L),
    PCtype                 = extract_txt(X1, 163L, 163L),
    Comm_Name              = extract_txt(X1, 164L, 193L),
    DMT                    = extract_txt(X1, 194L, 194L),
    H_DMT                  = extract_txt(X1, 195L, 195L),
    DMTDIFF                = extract_txt(X1, 196L, 196L),
    Birth_Date             = extract_dte(X1, 197L, 204L),
    Ret_Date               = extract_dte(X1, 205L, 212L),
    PO                     = extract_txt(X1, 213L, 213L),
    QI                     = extract_txt(X1, 214L, 216L),
    Source                 = extract_txt(X1, 217L, 217L),
    POP_CNTR_RA_SIZE_CLASS = extract_txt(X1, 218L, 218L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# uniq
input = list.files('data/txt', pattern='\\.UNIQ\\.', full.names = T)
output = 'data/csv/uniq.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE                  = extract_txt(X1, 1L, 6L),
    FSA                    = extract_txt(X1, 7L, 9L),
    PR                     = extract_txt(X1, 10L, 11L),
    CDuid                  = extract_txt(X1, 12L, 15L),
    CSDuid                 = extract_txt(X1, 16L, 22L),
    CSDname                = extract_txt(X1, 23L, 92L),
    CSDtype                = extract_txt(X1, 93L, 95L),
    CCScode                = extract_txt(X1, 96L, 98L),
    SAC                    = extract_txt(X1, 99L, 101L),
    SACtype                = extract_txt(X1, 102L, 102L),
    CTname                 = extract_txt(X1, 103L, 109L),
    ER                     = extract_txt(X1, 110L, 111L),
    DPL                    = extract_txt(X1, 112L, 115L),
    FED                    = extract_txt(X1, 116L, 118L),
    Pop_Cntr_RA            = extract_txt(X1, 121L, 124L),
    Pop_Cntr_RA_type       = extract_txt(X1, 125L, 125L),
    DAuid                  = extract_txt(X1, 126L, 133L),
    DB                     = extract_txt(X1, 134L, 136L),
    Rep_Pt_Type            = extract_txt(X1, 137L, 137L),
    LAT                    = extract_num(X1, 138L, 148L),
    LONG                   = extract_num(X1, 149L, 161L),
    SLI                    = extract_txt(X1, 162L, 162L),
    PCtype                 = extract_txt(X1, 163L, 163L),
    Comm_Name              = extract_txt(X1, 164L, 193L),
    DMT                    = extract_txt(X1, 194L, 194L),
    H_DMT                  = extract_txt(X1, 195L, 195L),
    DMTDIFF                = extract_txt(X1, 196L, 196L),
    Birth_Date             = extract_dte(X1, 197L, 204L),
    Ret_Date               = extract_dte(X1, 205L, 212L),
    PO                     = extract_txt(X1, 213L, 213L),
    QI                     = extract_txt(X1, 214L, 216L),
    Source                 = extract_txt(X1, 217L, 217L),
    POP_CNTR_RA_SIZE_CLASS = extract_txt(X1, 218L, 218L),
    nBLK                   = extract_num(X1, 219L, 219L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc2dups
input = list.files('data/txt', pattern='\\.WC2DUPS\\.', full.names = T)
output = 'data/csv/wc2dups.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE2  = extract_txt(X1, 1L, 2L),
    PR      = extract_txt(X1, 7L, 8L),
    CD      = extract_txt(X1, 9L, 10L),
    DA      = extract_txt(X1, 11L, 14L),
    CSDuid  = extract_txt(X1, 15L, 21L),
    SAC     = extract_txt(X1, 22L, 24L),
    CT      = extract_txt(X1, 25L, 31L),
    Tracted = extract_txt(X1, 32L, 32L),
    LAT     = extract_num(X1, 33L, 43L),
    LONG    = extract_num(X1, 44L, 56L),
    RPF     = extract_txt(X1, 57L, 57L),
    nDA     = extract_num(X1, 58L, 58L),
    nCD     = extract_num(X1, 59L, 59L),
    nCSD    = extract_num(X1, 60L, 60L),
    PC2DAWT = extract_num(X1, 61L, 67L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc2point
input = list.files('data/txt', pattern='\\.WC2POINT\\.', full.names = T)
output = 'data/csv/wc2point.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE2   = extract_txt(X1, 1L, 2L),
    FirstObs = extract_num(X1, 7L, 13L),
    nOBS     = extract_num(X1, 14L, 18L),
    TWT      = extract_num(X1, 19L, 25L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc3dups
input = list.files('data/txt', pattern='\\.WC3DUPS\\.', full.names = T)
output = 'data/csv/wc3dups.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE3  = extract_txt(X1, 1L, 3L),
    PR      = extract_txt(X1, 7L, 8L),
    CD      = extract_txt(X1, 9L, 10L),
    DA      = extract_txt(X1, 11L, 14L),
    CSDuid  = extract_txt(X1, 15L, 21L),
    SAC     = extract_txt(X1, 22L, 24L),
    CT      = extract_txt(X1, 25L, 31L),
    Tracted = extract_txt(X1, 32L, 32L),
    LAT     = extract_num(X1, 33L, 43L),
    LONG    = extract_num(X1, 44L, 56L),
    RPF     = extract_txt(X1, 57L, 57L),
    nDA     = extract_num(X1, 58L, 58L),
    nCD     = extract_num(X1, 59L, 59L),
    nCSD    = extract_num(X1, 60L, 60L),
    PC3DAWT = extract_num(X1, 61L, 67L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc3point
input = list.files('data/txt', pattern='\\.WC3POINT\\.', full.names = T)
output = 'data/csv/wc3point.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE3   = extract_txt(X1, 1L, 3L),
    FirstObs = extract_num(X1, 7L, 13L),
    nOBS     = extract_num(X1, 14L, 18L),
    TWT      = extract_num(X1, 19L, 25L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc4dups
input = list.files('data/txt', pattern='\\.WC4DUPS\\.', full.names = T)
output = 'data/csv/wc4dups.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE4  = extract_txt(X1, 1L, 4L),
    PR      = extract_txt(X1, 7L, 8L),
    CD      = extract_txt(X1, 9L, 10L),
    DA      = extract_txt(X1, 11L, 14L),
    CSDuid  = extract_txt(X1, 15L, 21L),
    SAC     = extract_txt(X1, 22L, 24L),
    CT      = extract_txt(X1, 25L, 31L),
    Tracted = extract_txt(X1, 32L, 32L),
    LAT     = extract_num(X1, 33L, 43L),
    LONG    = extract_num(X1, 44L, 56L),
    RPF     = extract_txt(X1, 57L, 57L),
    nDA     = extract_num(X1, 58L, 58L),
    nCD     = extract_num(X1, 59L, 59L),
    nCSD    = extract_num(X1, 60L, 60L),
    PC4DAWT = extract_num(X1, 61L, 67L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc4point
input = list.files('data/txt', pattern='\\.WC4POINT\\.', full.names = T)
output = 'data/csv/wc4point.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE4   = extract_txt(X1, 1L, 4L),
    FirstObs = extract_num(X1, 7L, 13L),
    nOBS     = extract_num(X1, 14L, 18L),
    TWT      = extract_num(X1, 19L, 25L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc5dups
input = list.files('data/txt', pattern='\\.WC5DUPS\\.', full.names = T)
output = 'data/csv/wc5dups.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE5  = extract_txt(X1, 1L, 5L),
    PR      = extract_txt(X1, 7L, 8L),
    CD      = extract_txt(X1, 9L, 10L),
    DA      = extract_txt(X1, 11L, 14L),
    CSDuid  = extract_txt(X1, 15L, 21L),
    SAC     = extract_txt(X1, 22L, 24L),
    CT      = extract_txt(X1, 25L, 31L),
    Tracted = extract_txt(X1, 32L, 32L),
    LAT     = extract_num(X1, 33L, 43L),
    LONG    = extract_num(X1, 44L, 56L),
    RPF     = extract_txt(X1, 57L, 57L),
    nDA     = extract_num(X1, 58L, 58L),
    nCD     = extract_num(X1, 59L, 59L),
    nCSD    = extract_num(X1, 60L, 60L),
    PC5DAWT = extract_num(X1, 61L, 67L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc5point
input = list.files('data/txt', pattern='\\.WC5POINT\\.', full.names = T)
output = 'data/csv/wc5point.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE5   = extract_txt(X1, 1L, 5L),
    FirstObs = extract_num(X1, 7L, 13L),
    nOBS     = extract_num(X1, 14L, 18L),
    TWT      = extract_num(X1, 19L, 25L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc6dups
input = list.files('data/txt', pattern='\\.WC6DUPS\\.', full.names = T)
output = 'data/csv/wc6dups.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE       = extract_txt(X1, 1L, 6L),
    PR          = extract_txt(X1, 7L, 8L),
    CD          = extract_txt(X1, 9L, 10L),
    DA          = extract_txt(X1, 11L, 14L),
    CSDuid      = extract_txt(X1, 15L, 21L),
    SAC         = extract_txt(X1, 22L, 24L),
    CT          = extract_txt(X1, 25L, 31L),
    Tracted     = extract_txt(X1, 32L, 32L),
    LAT         = extract_num(X1, 33L, 43L),
    LONG        = extract_num(X1, 44L, 56L),
    DMT         = extract_txt(X1, 57L, 57L),
    H_DMT       = extract_txt(X1, 58L, 58L),
    DMTDIFF     = extract_txt(X1, 59L, 59L),
    Rep_Pt_Type = extract_txt(X1, 60L, 60L),
    PCtype      = extract_txt(X1, 61L, 61L),
    nDA         = extract_num(X1, 62L, 62L),
    nCD         = extract_num(X1, 63L, 63L),
    nCSD        = extract_num(X1, 64L, 64L),
    PC6DAWT     = extract_num(X1, 65L, 71L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# wc6point
input = list.files('data/txt', pattern='\\.WC6POINT\\.', full.names = T)
output = 'data/csv/wc6point.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    PCODE    = extract_txt(X1, 1L, 6L),
    FirstObs = extract_num(X1, 7L, 13L),
    nOBS     = extract_num(X1, 14L, 18L),
    TWT      = extract_num(X1, 19L, 25L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# dablkpnt16
input = list.files('data/txt', pattern='\\.DABLKPNT16\\.', full.names = T)
output = 'data/csv/dablkpnt16.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    DAuid     = extract_txt(X1, 1L, 8L),
    PR        = extract_txt(X1, 1L, 2L),
    CD        = extract_txt(X1, 3L, 4L),
    DA        = extract_txt(X1, 5L, 8L),
    nBLK      = extract_num(X1, 9L, 12L),
    FirstObs  = extract_num(X1, 13L, 18L),
    DAPop2016 = extract_num(X1, 19L, 24L),
    SACcode   = extract_txt(X1, 26L, 28L),
    CSD       = extract_txt(X1, 30L, 32L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# gaf16
input = list.files('data/txt', pattern='\\.GAF16\\.', full.names = T)
output = 'data/csv/gaf16.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    DBuid         = extract_txt(X1, 1L, 11L),
    PR            = extract_txt(X1, 1L, 2L),
    CD            = extract_txt(X1, 3L, 4L),
    DA            = extract_txt(X1, 5L, 8L),
    DB            = extract_txt(X1, 9L, 10L),
    DBpop2016     = extract_txt(X1, 12L, 19L),
    DBtdwell2016  = extract_txt(X1, 20L, 27L),
    DB_ir2016     = extract_txt(X1, 28L, 28L),
    DAuid         = extract_txt(X1, 29L, 36L),
    DAlat         = extract_num(X1, 37L, 45L),
    DAlong        = extract_num(X1, 46L, 56L),
    PRuid         = extract_txt(X1, 57L, 58L),
    PRename       = extract_txt(X1, 59L, 88L),
    PRfname       = extract_txt(X1, 89L, 118L),
    FEDuid        = extract_txt(X1, 119L, 123L),
    FEDname       = extract_txt(X1, 124L, 208L),
    ERuid         = extract_txt(X1, 209L, 212L),
    ERname        = extract_txt(X1, 213L, 297L),
    CDuid         = extract_txt(X1, 298L, 301L),
    CDname        = extract_txt(X1, 302L, 341L),
    CSDuid        = extract_txt(X1, 342L, 348L),
    CSDname       = extract_txt(X1, 349L, 403L),
    SACtype       = extract_txt(X1, 404L, 404L),
    SACcode       = extract_txt(X1, 405L, 407L),
    CCSuid        = extract_txt(X1, 408L, 414L),
    CCSname       = extract_txt(X1, 415L, 469L),
    DPLuid        = extract_txt(X1, 470L, 475L),
    DPLname       = extract_txt(X1, 476L, 560L),
    DPLtype       = extract_txt(X1, 561L, 563L),
    CMAPuid       = extract_txt(X1, 564L, 568L),
    CMAname       = extract_txt(X1, 569L, 668L),
    CMAtype       = extract_txt(X1, 669L, 669L),
    CTuid         = extract_txt(X1, 670L, 679L),
    CTcode        = extract_txt(X1, 680L, 683L),
    CTname        = extract_txt(X1, 684L, 690L),
    PopCtrRAPuid  = extract_txt(X1, 691L, 696L),
    PopCtrRAname  = extract_txt(X1, 697L, 796L),
    PopCtrRAtype  = extract_txt(X1, 797L, 797L),
    PopCtrRAclass = extract_txt(X1, 798L, 798L),
    CARuid        = extract_txt(X1, 799L, 802L),
    CARname       = extract_txt(X1, 803L, 852L),
    DB11uid       = extract_txt(X1, 853L, 862L),
    DA11uid       = extract_txt(X1, 863L, 870L),
    DA06uid       = extract_txt(X1, 871L, 878L),
    DA01uid       = extract_txt(X1, 887L, 894L),
    EA96uid       = extract_txt(X1, 879L, 886L),
    EA91uid       = extract_txt(X1, 895L, 902L),
    EA86uid       = extract_txt(X1, 903L, 910L),
    EA81uid       = extract_txt(X1, 911L, 918L),
    CSize         = extract_txt(X1, 919L, 919L),
    CSizeMIZ      = extract_txt(X1, 920L, 920L),
    InuitLands    = extract_txt(X1, 921L, 921L),
    CSDtyp        = extract_txt(X1, 922L, 924L),
    DA21uid       = extract_txt(X1, 925L, 932L),
    DB21uid       = extract_txt(X1, 933L, 943L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# hrdef
input = list.files('data/txt', pattern='\\.HRDEF\\.', full.names = T)
output = 'data/csv/hrdef.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    DBuid    = extract_txt(X1, 1L, 11L),
    PR       = extract_txt(X1, 1L, 2L),
    CD       = extract_txt(X1, 3L, 4L),
    DA       = extract_txt(X1, 5L, 8L),
    DB       = extract_txt(X1, 9L, 11L),
    DAuid    = extract_txt(X1, 12L, 19L),
    CSD      = extract_txt(X1, 20L, 22L),
    HRuid    = extract_txt(X1, 27L, 30L),
    HRename  = extract_txt(X1, 31L, 90L),
    HRfname  = extract_txt(X1, 91L, 150L),
    AHRuid   = extract_txt(X1, 151L, 154L),
    AHRename = extract_txt(X1, 155L, 214L),
    AHRfname = extract_txt(X1, 215L, 274L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# ses16
input = list.files('data/txt', pattern='\\.SES16\\.', full.names = T)
output = 'data/csv/ses16.csv'

cat('Processing ', input, '...\n', sep = '')

read_tsv(file = input, col_names = F, show_col_types = F) %>%
  mutate(
    DAUID    = extract_txt(X1, 1L, 8L),
    AREA     = extract_txt(X1, 10L, 12L),
    IMPFLG   = extract_txt(X1, 13L, 13L),
    BTIPPE   = extract_num(X1, 14L, 21L),
    ATIPPE   = extract_num(X1, 23L, 30L),
    QABTIPPE = extract_txt(X1, 32L, 32L),
    QNBTIPPE = extract_txt(X1, 34L, 34L),
    DABTIPPE = extract_txt(X1, 36L, 37L),
    DNBTIPPE = extract_txt(X1, 39L, 40L),
    QAATIPPE = extract_txt(X1, 42L, 42L),
    QNATIPPE = extract_txt(X1, 44L, 44L),
    DAATIPPE = extract_txt(X1, 46L, 47L),
    DNATIPPE = extract_txt(X1, 49L, 50L)) %>%
  select(!X1) %>%
  write_csv(file = output)

rm(input, output) 


# Clean up
rm(extract_txt, extract_num)
