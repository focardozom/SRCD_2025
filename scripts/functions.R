# Packages

# List of required packages
required_packages <- c("tidyverse", "here", "gtsummary")

# Check and install packages if not already installed
if (!require("pak")) install.packages("pak")
pak::pak(required_packages)

# Files

dictionary <- readxl::read_xlsx(here("data", "DICTIONARY.xlsx"), sheet = 2) |>
  mutate(`variable name` = trimws(`variable name`))

dictionary_match <- readxl::read_xlsx(here("data", "DICTIONARY.xlsx"), sheet = 3)


# Select variables --------------------------------------------------------

select_variables <- function(x, y) {
  variables <- dictionary |>
    filter(variables %in% x) |>
    filter(str_detect(`variable name`, paste0("^", y))) |>
    select(`variable name`) |>
    pull(`variable name`)

  return(variables)
}

# Translator --------------------------------------------------------------

translator <- function(x, y) {
  vars <- select_variables(x, y)

  translator <-
    dictionary |>
    filter(`variable name` %in% vars) |>
    filter(str_detect(`variable name`, paste0("^", y))) |>
    select(`variable name`, description) |>
    filter(!is.na(description)) |>
    mutate(qid_dict = str_sub(`variable name`, 3)) |>
    mutate(label = paste0(qid_dict, " - ", description))

  fun_translator_f <- setNames(translator$label, paste0(translator$qid_dict, "$"))

  return(fun_translator_f)
}

# Match variables ------------------------------------------------------

match1 <- dictionary_match |>
  select(`variable name`, match1, match2, combined) |>
  filter(!is.na(match1))

match2 <- dictionary_match |>
  select(`variable name`, match1, match2, combined) |>
  filter(!is.na(match2))

dic_match1 <- setNames(match1$`variable name`, paste0(match1$match1, "$"))
dic_match2 <- setNames(match2$`variable name`, paste0(match2$match2, "$"))

# Time table --------------------------------------------------------------

time_table <- function(x, y) {
  vars <- select_variables(x, y)
  translate <- translator(x, y)

  the_data |>
    select(idnum, all_of(vars)) |>
    select(idnum, starts_with(y)) |>
    pivot_longer(-idnum, values_transform = as.character) |>
    mutate(name = str_replace_all(name, dic_match1)) |> # match1
    mutate(name = str_replace_all(name, dic_match2)) |> # match2
    mutate(number = str_extract(value, "-?\\d+")) |>
    mutate(wave = str_extract(name, "(?<=^.).")) |>
    mutate(qid = str_sub(name, 3)) |>
    mutate(label = str_replace_all(qid, translate)) |>
    mutate(label = gsub("\\(.*?\\)", "", label)) |>
    filter(number >= 0) |> # keep the number 0
    pivot_wider(
      id_cols = c(idnum, wave),
      values_from = value,
      names_from = label
    ) |>
    select(-idnum) |>
    tbl_strata(
      strata = wave,
      .tbl_fun = ~ .x |>
        tbl_summary(
          missing = "no"
        )
    )
}

time_table_continuas <- function(x, y) {
  vars <- select_variables(x, y)
  translate <- translator(x, y)

  the_data |>
    select(idnum, all_of(vars)) |>
    select(idnum, starts_with(y)) |>
    pivot_longer(-idnum, values_transform = as.character) |>
    mutate(name = str_replace_all(name, dic_match1)) |> # match1
    mutate(name = str_replace_all(name, dic_match2)) |> # match2
    mutate(number = str_extract(value, "-?\\d+")) |>
    mutate(wave = str_extract(name, "(?<=^.).")) |>
    mutate(qid = str_sub(name, 3)) |>
    mutate(label = str_replace_all(qid, translate)) |>
    mutate(label = gsub("\\(.*?\\)", "", label)) |>
    filter(number >= 0) |> # keep the number 0
    mutate(value = as.numeric(value)) |>
    pivot_wider(
      id_cols = c(idnum, wave),
      values_from = value,
      names_from = label
    ) |>
    select(-idnum) |>
    tbl_strata(
      strata = wave,
      .tbl_fun = ~ .x |>
        tbl_summary(
          missing = "no"
        )
    )
}

# Combine functions -------------------------------------------------------

do_all <- function(x) {
  time_teable_f <- time_table(x, "f")
  time_teable_m <- time_table(x, "m")

  tables <- list(time_teable_f, time_teable_m)
}

# Create combined document

library(openxlsx)
create_combined_document <- function(x) {
  files <- list.files("output", pattern = "xlsx", full.names = TRUE)
  file_names <- sapply(strsplit(basename(files), "\\."), `[`, 1) # This removes the extension
  file <- setNames(lapply(files, read.xlsx), file_names)
  writexl::write_xlsx(file, "combined_excel_file.xlsx")
}


# Demographics table ------------------------------------------------------

## Create table
demo_table <- function(x) {
  the_data |>
    select(idnum, all_of(demographics)) |>
    select(idnum, starts_with(x)) |>
    pivot_longer(-idnum, values_transform = as.character) |>
    mutate(number = str_extract(value, "-?\\d+")) |>
    mutate(wave = str_extract(name, "(?<=^.).")) |>
    mutate(labels = str_replace_all(name, dict_labels)) |>
    mutate(labels = paste0(name, " - ", labels)) |>
    pivot_wider(id_cols = idnum, names_from = labels, values_from = value) |>
    tbl_summary(include = -idnum)
}


# Depression functions ----------------------------------------------------

do_all_depression <- function(x, y) {
  vars <- select_variables(x, y)
  translator <- dictionary |>
    filter(`variable name` %in% vars) |>
    filter(str_detect(`variable name`, paste0("^", y))) |>
    select(`variable name`, description) |>
    filter(!is.na(description)) |>
    mutate(qid_dict = str_sub(`variable name`, 3)) |>
    mutate(label = paste0(`variable name`, " - ", description))

  fun_trans <- setNames(translator$label, paste0(translator$`variable name`, "$"))

  the_data |>
    select(idnum, all_of(vars)) |>
    select(idnum, starts_with(y)) |>
    pivot_longer(-idnum, values_transform = as.character) |>
    mutate(name = str_replace_all(name, dic_match1)) |>
    mutate(name = str_replace_all(name, dic_match2)) |>
    mutate(number = str_extract(value, "-?\\d+")) |>
    mutate(wave = str_extract(name, "(?<=^.).")) |>
    mutate(qid = str_sub(name, 3)) |>
    mutate(label = str_replace_all(name, fun_trans)) |>
    mutate(label = gsub("\\(.*?\\)", "", label)) |>
    filter(number >= 0) |>
    pivot_wider(
      id_cols = c(idnum, wave),
      values_from = value,
      names_from = label
    ) |>
    select(-idnum) |>
    tbl_strata(
      strata = wave,
      .tbl_fun = ~ .x |>
        tbl_summary(
          missing = "no"
        )
    )
}


# Export tables to Mplus --------------------------------------------------


export_single_table <- function(x, y, z) {
  vars <- select_variables(x, y)

  table <- the_data |>
    select(idnum, all_of(vars)) |>
    select(idnum, starts_with(y)) |>
    arrange(idnum) |>
    pivot_longer(-idnum, values_transform = as.character) |>
    mutate(name = str_replace_all(name, dic_match1)) |> # match1
    mutate(name = str_replace_all(name, dic_match2)) |> # match2
    mutate(number = str_extract(value, "-?\\d+")) |>
    mutate(wave = str_extract(name, "(?<=^.).")) |>
    filter(number >= 0) |> # keep the number 0
    pivot_wider(
      id_cols = c(idnum),
      values_from = number,
      names_from = name
    ) |>
    mutate(across(-idnum, ~ case_when(is.na(.x) ~ "999999", TRUE ~ .x))) |>
    mutate(across(where(is.character), ~ as.numeric(.x)))

  construct <- gsub(" ", "_", x)
  cat(
    MplusAutomation::
    prepareMplusData(table,
      filename =
        glue::glue("output/Mplus/{z}/data_{construct}_{y}_.dat")
    ),
    file = glue::glue("output/Mplus/{z}/model_{construct}_{y}_.txt")
  )
}

export_tables <- function(x, z) {
  export_single_table(x, "f", z)
  export_single_table(x, "m", z)
}

export_single_table_combine <- function(x, y, z) {
  vars <- select_variables(x, y)

  table <- the_data |>
    select(idnum, all_of(vars)) |>
    select(idnum, starts_with(y)) |>
    arrange(idnum) |>
    pivot_longer(-idnum, values_transform = as.character) |>
    mutate(name = str_replace_all(name, dic_match1)) |> # match1
    mutate(name = str_replace_all(name, dic_match2)) |> # match2
    mutate(number = str_extract(value, "-?\\d+")) |>
    mutate(wave = str_extract(name, "(?<=^.).")) |>
    filter(number >= 0) |> # keep the number 0
    pivot_wider(
      id_cols = c(idnum),
      values_from = number,
      names_from = name
    ) |>
    mutate(across(-idnum, ~ case_when(is.na(.x) ~ "999999", TRUE ~ .x))) |>
    mutate(across(where(is.character), ~ as.numeric(.x)))

  cat(
    MplusAutomation::
    prepareMplusData(table,
      filename =
        glue::glue("output/Mplus/{z}/data_combine_.dat")
    ),
    file = glue::glue("output/Mplus/{z}/model_combine_.txt")
  )
}

export_tables_coparenting <- function(x, z) {
  export_single_table(x, "f", z)
  export_single_table(x, "m", z)
}

select_variables_test <- function(x) {
  variables <- dictionary |>
    filter(variables %in% x) |>
    filter(str_detect(
      `variable name`,
      paste0("^(f|m)")
    )) |>
    select(`variable name`) |>
    pull(`variable name`)

  return(variables)
}

export_single_table_coparenting <- function(x, z) {
  vars <- select_variables_test(x)

  table <- the_data |>
    select(idnum, all_of(vars)) |>
    select(idnum, starts_with("f"), starts_with("m")) |>
    arrange(idnum) |>
    pivot_longer(-idnum, values_transform = as.character) |>
    mutate(name = str_replace_all(name, dic_match1)) |> # match1
    mutate(name = str_replace_all(name, dic_match2)) |> # match2
    mutate(number = str_extract(value, "-?\\d+")) |>
    mutate(wave = str_extract(name, "(?<=^.).")) |>
    filter(number >= 0) |> # keep the number 0
    pivot_wider(
      id_cols = c(idnum),
      values_from = number,
      names_from = name
    ) |>
    mutate(across(-idnum, ~ case_when(is.na(.x) ~ "999999", TRUE ~ .x))) |>
    mutate(across(where(is.character), ~ as.numeric(.x)))

  construct <- gsub(" ", "_", x)
  cat(
    MplusAutomation::
    prepareMplusData(table,
      filename =
        glue::glue("output/Mplus/{z}/data_combined_{construct}.dat")
    ),
    file = glue::glue("output/Mplus/{z}/model_combined_{construct}.txt")
  )
}


# Clean to merge

clean_to_merge <- function(x) {
  variables <- dictionary |>
    filter(variables %in% x) |>
    select(`variable name`) |>
    pull(`variable name`)

  table <- the_data |>
    select(idnum, all_of(variables)) |>
    pivot_longer(-idnum, values_transform = as.character) |>
    mutate(name = str_replace_all(name, dic_match1)) |> # match1
    mutate(name = str_replace_all(name, dic_match2)) |> # match2
    mutate(number = str_extract(value, "-?\\d+")) |>
    mutate(wave = str_extract(name, "(?<=^.).")) |>
    filter(number >= 0) |> # keep the number 0
    pivot_wider(
      id_cols = c(idnum),
      values_from = value,
      names_from = name
    )
}
