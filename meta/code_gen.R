# Using the appropriate data dictionary and raw data file, produce code for converting raw PCCF+ data files into tables
extraction_code_gen = function(datadict, name, output = "", append_output = T){
  maxncharname = max(nchar(datadict$Name))
  code1 = datadict %>%
    mutate(
      numspaces = pmax(maxncharname - nchar(Name), 0),
      spaces = strrep(" ", numspaces),
      Code = case_when(
        str_detect(Name, "_Date") ~
          str_glue("{Name}{spaces} = extract_dte(X1, {Start}L, {End}L)"),
        Type == "C" ~
          str_glue("{Name}{spaces} = extract_txt(X1, {Start}L, {End}L)"),
        Type == "N" ~
          str_glue("{Name}{spaces} = extract_num(X1, {Start}L, {End}L)"))) %>%
    use_series("Code") %>%
    paste0(collapse = ",\n    ")
  
    str_glue(
      "",
      "# {name}",
      "input = list.files('data/txt', pattern='\\\\.{toupper(name)}\\\\.', full.names = T)",
      "output = 'data/csv/{name}.csv'",
      "",
      "cat('Processing ', input, '...\\n', sep = '')",
      "",
      "read_tsv(file = input, col_names = F, show_col_types = F) %>%",
      "  mutate(",
      "    {code1}) %>%",
      "  select(!X1) %>%",
      "  write_csv(file = output)",
      "",
      "rm(input, output)",
      .sep="\n") %>%
    cat("\n\n\n", file = output, append = append_output)
}