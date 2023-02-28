# Converts copied data dictionary text from PDF manual into tables
#
# Text Directly copied from:
# Postal CodeOM Conversion File Plus (PCCF+) Version 7E, Reference Guide


parse_data_dictionary = function(x){
  x %>%
    str_remove_all("^\\s+") %>%
    str_remove_all("(?<=\\n)\\s+") %>%
    str_split("\n") %>% 
    extract2(1) %>%
    str_match("^(\\d+)\\s+(\\d+\\.{0,1}\\d*)\\s+(\\w)\\s+(\\w)\\s+(\\w+)\\s([\\w\\s\\d\\(\\)/'-\\.\\+\\:\\=]+)$") %>%
    magrittr::extract(, 2:7) %>%
    set_colnames(c("Start", "Format", "Type", "Read by PCCF+", "Name", "Description")) %>%
    as_tibble %>%
    mutate(
      Start   = as.integer(Start),
      Length  = as.integer(str_extract(Format, "\\d+")),
      Decimal = replace_na(as.integer(str_extract(Format, "(?<=\\.)\\d+")) + 1L, 0L),
      End1    = Start + Length + Decimal - 1L,
      End2    = if_else(lead(Start, 1L) > Start, lead(Start, 1L) - 1L, NA_integer_),
      End     = pmin(End1, End2, na.rm=T)) %>%
    select(Start, End, Type, Name, Description) %>%
    return
}

data_dictionary = list()

data_dictionary[["dups"]] = "
  1 6 C Y PCODE Postal codeOM
  7 3 C Y FSA Forward Sortation Area
  10 2 C Y PR Province code
  12 4 C Y CDuid Census Division code
  16 7 C Y CSDuid Census Subdivision code
  23 70 C Y CSDname Census Subdivision name
  93 3 C Y CSDtype Census Subdivision type
  96 3 C Y CCScode Census Consolidated Subdivision code
  99 3 C Y SAC Statistical Area Classification code
  102 1 C Y SACtype Statistical Area Classification type
  103 7 C Y CTname Census Tract name
  110 2 C Y ER Economic Region identifier
  112 4 C Y DPL Designated Place identifier
  116 5 C Y FEDuid Federal Electoral District (2003) unique identifier
  121 4 C Y Pop_Cntr_RA Population centre/rural area code
  125 1 C Y Pop_Cntr_RA_type Population centre/rural area type
  126 8 C Y DAuid Dissemination area code
  134 3 C Y DB Dissemination block code
  137 1 C Y Rep_Pt_Type Representative Point Type
  138 11.6 N Y LAT Latitude of lowest level geographic area
  149 13.6 N Y LONG Longitude of lowest level geographic area
  162 1 C Y SLI Single Link Indicator
  163 1 C Y PCtype Postal codeOM type
  164 30 C Y Comm_Name Canada Post Community Name
  194 1 C Y DMT Delivery Mode Type
  195 1 C Y H_DMT Historic Delivery Mode Type
  196 1 C Y DMTDIFF Previous or alternate DMT (if applicable)
  197 8 C Y Birth_Date Birth date of postal codeOM (yyyymmdd)
  205 8 C Y Ret_Date Retirement date of postal codeOM (yyyymmdd)
  213 1 C Y PO Delivery installation
  214 3 C Y QI Quality indicator
  217 1 C Y Source Source of postal code geocoding (PCCF)
  218 1 C Y Pop_Cntr_RA_Size_Class Population centre/rural area classification
  219 1 N Y nCD Number of census divisions for this Postal CodeOM (1-9+)
  220 1 N Y nCSD Number of census subdivisions for this postal codeOM (1-9+)
  221 1 N Y nDA Number of dissemination areas for this postal codeOM (1-9+)
  222 1 N Y nDB Number of dissemination blocks for this postal codeOM (1-9+)
  223 1 N N keeprec Postal code has both retired and rebirthed records"

data_dictionary[["pointdup"]] = "
  1 6 C Y PCODE Postal codeOM
  7 4 N Y nPCODE Number of records for this postal codeOM
  11 8 N Y ObsDup Observation number for first occurrence on duplicates file"

data_dictionary[["rpo"]] = "
  1 6 C Y PCODE Postal codeOM
  7 3 C Y FSA Forward Sortation Area
  10 2 C Y PR Province code
  12 4 C Y CDuid Census Division code
  16 7 C Y CSDuid Census Subdivision code
  23 70 C Y CSDname Census Subdivision name
  93 3 C Y CSDtype Census Subdivision type
  96 3 C Y CCScode Census Consolidated Subdivision code
  99 3 C Y SAC Statistical Area Classification code
  102 1 C Y SACtype Statistical Area Classification type
  103 7 C Y CTname Census Tract name
  110 2 C Y ER Economic Region identifier
  112 4 C Y DPL Designated Place identifier
  116 3 C Y FED Federal Electoral District (2013) unique identifier
  121 4 C Y Pop_Cntr_RA Population centre/rural area code
  125 1 C Y Pop_Cntr_RA_type Population centre/rural area type
  126 8 C Y DAuid Dissemination area code
  134 3 C Y DB Dissemination block code
  137 1 C Y Rep_Pt_Type Representative Point Type
  138 11.6 N Y LAT Latitude of lowest level geographic area
  149 13.6 N Y LONG Longitude of lowest level geographic area
  162 1 C Y SLI Single Link Indicator
  163 1 C Y PCtype Postal codeOM type
  164 30 C Y Comm_Name Canada Post Community Name
  194 1 C Y DMT Delivery Mode Type
  195 1 C Y H_DMT Historic Delivery Mode Type
  196 1 C Y DMTDIFF Previous or alternate DMT (if applicable)
  197 8 C Y Birth_Date Birth date of postal codeOM (yyyymmdd)
  205 8 C Y Ret_Date Retirement date of postal codeOM (yyyymmdd)
  213 1 C Y PO Delivery installation
  214 3 C Y QI Quality indicator
  217 1 C Y Source Source of postal codeOM geocoding (PCCF)
  218 1 C Y POP_CNTR_RA_SIZE_CLASS Population centre/rural area classification"

data_dictionary[["uniq"]] = "
  1 6 C Y PCODE Postal codeOM
  7 3 C Y FSA Forward Sortation Area
  10 2 C Y PR Province code
  12 4 C Y CDuid Census Division code
  16 7 C Y CSDuid Census Subdivision code
  23 70 C Y CSDname Census Subdivision name
  93 3 C Y CSDtype Census Subdivision type
  96 3 C Y CCScode Census Consolidated Subdivision code
  99 3 C Y SAC Statistical Area Classification code
  102 1 C Y SACtype Statistical Area Classification type
  103 7 C Y CTname Census Tract name
  110 2 C Y ER Economic Region identifier
  112 4 C Y DPL Designated Place identifier
  116 3 C Y FED Federal Electoral District (2013) unique identifier
  121 4 C Y Pop_Cntr_RA Population centre/rural area code
  125 1 C Y Pop_Cntr_RA_type Population centre/rural area type
  126 8 C Y DAuid Dissemination area code
  134 3 C Y DB Dissemination block code
  137 1 C Y Rep_Pt_Type Representative Point Type
  138 9.6 N Y LAT Latitude of lowest level geographic area
  149 11.6 N Y LONG Longitude of lowest level geographic area
  162 1 C Y SLI Single Link Indicator
  163 1 C Y PCtype Postal code type
  164 30 C Y Comm_Name Canada Post Community Name
  194 1 C Y DMT Delivery Mode Type
  195 1 C Y H_DMT Historic Delivery Mode Type
  196 1 C Y DMTDIFF Previous or alternate DMT (if applicable)
  197 8 C Y Birth_Date Birth date of postal codeOM (yyyymmdd)
  205 8 C Y Ret_Date Retirement date of postal codeOM (yyyymmdd)
  213 1 C Y PO Delivery installation
  214 3 C Y QI Quality indicator
  217 1 C Y Source Source of postal codeOM geocoding (PCCF)
  218 1 C Y POP_CNTR_RA_SIZE_CLASS Population centre/rural area classification
  219 1 N Y nBLK Number of dissemination blocks for this postal codeOM (1-9+)"

data_dictionary[["wc2dups"]] = "
  1 2 C Y PCODE2 First 2 characters of postal codeOM
  7 2 C Y PR Province code
  9 2 C Y CD Census Division code
  11 4 C Y DA Dissemination Area code
  15 7 C Y CSDuid Census subdivision code
  22 3 C Y SAC Statistical Area Classification code
  25 7 C Y CT Census Tract name
  32 1 C Y Tracted Flag for census tracted (DA)
  33 9.6 N Y LAT Latitude of dissemination area centroid
  44 11.6 N Y LONG Longitude of dissemination area centroid
  57 1 C Y RPF Representative point flag (PCCF+)
  58 1 N Y nDA Number of dissemination areas for PCODE2
  59 1 N Y nCD Number of census divisions for PCODE2
  60 1 N Y nCSD Number of census subdivisions for PCODE2
  61 4.2 N Y PC2DAWT Dissemination area level weight for PCODE2"

data_dictionary[["wc2point"]] = "
  1 2 C Y PCODE2 First 2 characters of postal codeOM
  7 7 N Y FirstObs Pointer for first observation on wc2dups
  14 5 N Y nOBS Total number of observations for first 2 characters
  19 4.2 N Y TWT 2 character population weight"

data_dictionary[["wc3dups"]] = "
  1 3 C Y PCODE3 First 3 characters of postal codeOM
  7 2 C Y PR Province code
  9 2 C Y CD Census Division code
  11 4 C Y DA Dissemination Area code
  15 7 C Y CSDuid Census subdivision code
  22 3 C Y SAC Statistical Area Classification
  25 7 C Y CT Census Tract name
  32 1 C Y Tracted Flag for census tracted (DA)
  33 9.6 N Y LAT Latitude of dissemination area centroid
  44 11.6 N Y LONG Longitude of dissemination area centroid
  57 1 C Y RPF Representative point flag (PCCF+)
  58 1 N Y nDA Number of dissemination areas for PCODE3
  59 1 N Y nCD Number of census divisions for PCODE3
  60 1 N Y nCSD Number of census subdivisions for PCODE3
  61 4.2 N Y PC3DAWT Dissemination area level weight for PCODE3"

data_dictionary[["wc3point"]] = "
  1 3 C Y PCODE3 (FSA) Forward Sortation Area
  7 7 N Y FirstObs Pointer for first observation on wc3dups
  14 5 N Y nOBS Total number of observations for FSA
  19 4.2 N Y TWT 3 character population weight"

data_dictionary[["wc4dups"]] = "
  1 4 C Y PCODE4 First 4 characters of postal codeOM
  7 2 C Y PR Province code
  9 2 C Y CD Census Division code
  11 4 C Y DA Dissemination Area code
  15 7 C Y CSDuid Census subdivision code
  22 3 C Y SAC Statistical Area Classification
  25 7 C Y CT Census Tract name
  32 1 C Y Tracted Flag for census tracted (DA)
  33 9.6 N Y LAT Latitude of dissemination area centroid
  44 11.6 N Y LONG Longitude of dissemination area centroid
  57 1 C Y RPF Representative point flag (PCCF+)
  58 1 N Y nDA Number of dissemination areas for PCODE4
  59 1 N Y nCD Number of census divisions for PCODE4
  60 1 N Y nCSD Number of census subdivisions for PCODE4
  61 4.2 N Y PC4DAWT Dissemination area level weight for PCODE4"

data_dictionary[["wc4point"]] = "
  1 4 C Y PCODE4 First 4 characters of postal codeOM
  7 7 N Y FirstObs Pointer for first observation on wc4dups
  14 5 N Y nOBS Total number of observations for PCODE4
  19 4.2 N Y TWT Total weight"

data_dictionary[["wc5dups"]] = "
  1 5 C Y PCODE5 First 5 characters of postal codeOM
  7 2 C Y PR Province code
  9 2 C Y CD Census Division code
  11 4 C Y DA Dissemination Area code
  15 7 C Y CSDuid Census subdivision code
  22 3 C Y SAC Statistical Area Classification
  25 7 C Y CT Census Tract name
  32 1 C Y Tracted DA is census tracted
  33 9.6 N Y LAT Latitude of dissemination area centroid
  44 11.6 N Y LONG Longitude of dissemination area centroid
  57 1 C Y RPF Representative point flag
  58 1 N Y nDA Number of dissemination areas PCODE5
  59 1 N Y nCD Number of census divisions for PCODE5
  60 1 N Y nCSD Number of census subdivisions for PCODE5
  61 4.2 N Y PC5DAWT Dissemination area level weight for PCODE5"

data_dictionary[["wc5point"]] = "
  1 5 C Y PCODE5 First 5 characters of postal codeOM
  7 7 N Y FirstObs Pointer for first observation on wc5dups
  14 5 N Y nOBS Total number of observations for PCODE5
  19 4.2 N Y TWT Total weight"

data_dictionary[["wc6dups"]] = "
  1 6 C Y PCODE 6 character postal codeOM
  7 2 C Y PR Province code
  9 2 C Y CD Census Division code
  11 4 C Y DA Dissemination Area code
  15 7 C Y CSDuid Census subdivision code
  22 3 C Y SAC Statistical Area Classification
  25 7 C Y CT Census Tract name
  32 1 C Y Tracted Census tracted DA
  33 9.6 N Y LAT Latitude of dissemination area centroid
  44 11.6 N Y LONG Longitude of dissemination area centroid
  57 1 C Y DMT Delivery Mode Type
  58 1 C Y H_DMT Historic Delivery Mode Type
  59 1 C Y DMTDIFF Previous DMT if different from current DMT
  60 1 C Y Rep_Pt_Type Representative Point Type
  61 1 C Y PCtype Postal codeOM type
  62 1 N Y nDA Number of dissemination areas for PCODE
  63 1 N Y nCD Number of census divisions for PCODE
  64 1 N Y nCSD Number of census subdivisions for PCODE
  65 4.2 N Y PC6DAWT Dissemination area level weight for PCODE"

data_dictionary[["wc6point"]] = "
  1 6 C Y PCODE 6 character postal codeOM
  7 7 N Y FirstObs Pointer for first observation on wc6dups
  14 5 N Y nOBS Total number of observations for PCODE
  19 4.2 N Y TWT Total weight"

data_dictionary[["dablkpnt16"]] = "
  1 8 C Y DAuid Dissemination area unique identifier
  1 2 C Y PR Province
  3 2 C Y CD Census division
  5 4 C Y DA Dissemination area
  9 4 N Y nBLK Number of dissemination blocks in DA
  13 6 N Y FirstObs Observation of first block occurrence
  19 6 N Y DAPop2016 Sum of DB population within DA
  26 3 C Y SACcode Statistical Area Classification code
  30 3 C Y CSD Census subdivision"

data_dictionary[["gaf16"]] = "
  1 11 C Y DBuid Dissemination block unique identifier
  1 2 C Y PR Province or territory code
  3 2 C Y CD Census division
  5 4 C Y DA Dissemination area
  9 2 C Y DB Dissemination block
  12 8 C Y DBpop2016 Dissemination block population (rounded)
  20 8 C Y DBtdwell2016 Dissemination block total private dwellings
  28 1 C Y DB_ir2016 Indian Reserve flag
  29 8 C Y DAuid Dissemination area (2016) unique identifier
  37 9.6 N Y DAlat Dissemination area latitude coordinate (decimal degrees)
  46 11.6 N Y DAlong Dissemination area longitude coordinate (decimal degrees)
  57 2 C N PRuid Province or territory
  59 30 C Y PRename Province or territory name in English
  89 30 C Y PRfname Province or territory name in French
  119 5 C Y FEDuid Federal Electoral District (2013) unique identifier
  124 85 C Y FEDname Federal Electoral District (2013) name
  209 4 C Y ERuid Economic Region unique identifier
  213 85 C Y ERname Economic Region name
  298 7 C Y CDuid Census Division unique identifier
  302 40 C Y CDname Census Division name
  342 7 C Y CSDuid Census Subdivision unique identifier
  349 55 C Y CSDname Census Subdivision name
  404 1 C Y SACtype Statistical Area Classification type
  405 3 C Y SACcode Statistical Area Classification code
  408 7 C Y CCSuid Census Consolidated Subdivision unique identifier
  415 55 C Y CCSname Census Consolidated Sub-division name
  470 6 C Y DPLuid Designated Place unique identifier
  476 85 C Y DPLname Designated Place name
  561 3 C Y DPLtype Designated Place type
  564 5 C Y CMAPuid Census Metropolitan Area unique identifier
  569 100 C Y CMAname Census Metropolitan Area name
  669 1 C Y CMAtype Census Metropolitan Area type
  670 10 C Y CTuid Census Tract unique identifier within a census metropolitan / agglomeration area
  680 4 C Y CTcode Census Tract unique identifier
  684 7 C Y CTname Census Tract 7.2 character numeric 'name'
  691 6 C Y PopCtrRAPuid Population Centre and Rural Area unique identifier
  697 100 C Y PopCtrRAname Population Centre and Rural Area name
  797 1 C Y PopCtrRAtype Population Centre and Rural Area type
  798 1 C Y PopCtrRAclass Population Centre and Rural Area class
  799 4 C Y CARuid Census Agricultural Area unique identifier
  803 50 C Y CARname Census Agricultural Area name
  853 10 C Y DB11uid Dissemination block (2011)
  863 8 C Y DA11uid Dissemination block (2011)
  871 8 C Y DA06uid Dissemination area (2006)
  887 8 C Y DA01uid Dissemination area (2001)
  879 8 C Y EA96uid Enumeration Area (1996)
  895 8 C Y EA91uid Enumeration Area (1991)
  903 8 C Y EA86uid Enumeration Area (1986)
  911 8 C Y EA81uid Enumeration Area (1981)
  919 1 C Y CSize Community Size Code
  920 1 C Y CSizeMIZ Community Size with MIZ
  921 1 C Y InuitLands Inuit Nunangat land claim regions
  922 3 C Y CSDtyp Census Subdivision type
  925 8 C Y DA21uid Dissemination Areas (2021)
  933 11 C Y DB21uid Dissemination Block (2021)"

data_dictionary[["hrdef"]] = "
  1 11 C N DBuid Dissemination block code
  1 2 C Y PR Province code
  3 2 C Y CD Census Division code
  5 4 C Y DA Dissemination Area code
  9 3 C Y DB Dissemination Block code
  12 8 C N DAuid Dissemination Area code
  20 3 C Y CSD Census subdivision code
  27 4 C Y HRuid Health Region unique identifier
  31 60 C Y HRename Health Region name (English)
  91 60 C Y HRfname Health Region name (French)
  151 4 C Y AHRuid Alternate Health Region unique identifier
  155 60 C Y AHRename Alternate Health Region name (English)
  215 60 C Y AHRfname Alternate Health Region name (French)"

data_dictionary[["ses16"]] = "
  1 8 C Y DAUID Dissemination Area unique identifier
  10 3 C Y AREA CMA/CA identifier or: CMA/CA code or R+PR (Ex : R24 = Quebec)
  13 1 C Y IMPFLG Imputation flag for IPPE (mean household income imputed)
  14 8 N Y BTIPPE Neighbourhood before tax income per single person equivalent (BTIPPE)
  23 8 N Y ATIPPE Neighbourhood after tax income per single person equivalent (ATIPPE)
  32 1 C Y QABTIPPE Neighbourhood income quintile before tax (in CMA/CA)
  34 1 C Y QNBTIPPE Neighbourhood income quintile before tax (national)
  36 2 C Y DABTIPPE Neighbourhood income decile before tax (in CMA/CA)
  39 2 C Y DNBTIPPE Neighbourhood income decile before tax (national)
  42 1 C Y QAATIPPE Neighbourhood income quintile after tax (in CMA/CA)
  44 1 C Y QNATIPPE Neighbourhood income quintile after tax (national)
  46 2 C Y DAATIPPE Neighbourhood income decile after tax (in CMA/CA)
  49 2 C Y DNATIPPE Neighbourhood income decile after tax (national)"

data_dictionary %<>% lapply(parse_data_dictionary)

rm(parse_data_dictionary)