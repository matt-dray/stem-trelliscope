# Test: Trelliscope for interactive small multiples
# See https://hafen.github.io/trelliscopejs/index.html by Ryan Hafen
# Matt Dray
# Jan 2018, updated November 2018


# Load packages -----------------------------------------------------------


library(dplyr)  # tidy data manipulation
library(readxl)  # read Excel files
library(tidyr)   # tidy dataframes
library(ggplot2)  # tidy plots
library(janitor)  # misc tidy convenience functions
library(rbokeh)  # R interface to Bokeh plotting library
library(trelliscopejs)  # small multiples


# School-level data -------------------------------------------------------


# Download file

download.file(
  url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/593438/SFR05_2017_Maths_and_science_tables_13_and_14_updated.xlsm",
  destfile = "data/maths_science_2017.xlsm"
)

# Read raw data

stem_sch_raw <- readxl::read_excel(
  "data/maths_science_2017.xlsm",
  sheet = "Table 13b",  # worksheet title
  skip = 7,  # column names in eighth row
  na = c("", "x", "NE", "NEW")  # NA values
) 

# Wrangle data

stem_sch <- stem_sch_raw %>%
  janitor::clean_names() %>%  # make column names sensible
  dplyr::select(
    urn = unique_reference_number_urn,
    sch_name = institution,
    region,
    la_name = local_authority,
    math = mathematics,
    fmath = further_mathematics,
    biol = biological_sciences,
    chem = chemistry,
    phys = physics,
    comp = computing) %>%
  dplyr::filter(!is.na(la_name)) %>%  # remove rows without LA name
  dplyr::mutate(urn = as.character(urn)) %>% 
  tidyr::gather(key = subject, value = entry_sch, math:comp)


# LA-level data -----------------------------------------------------------


# Read raw data
  
stem_la_raw <- readxl::read_excel(
  "data/SFR05_2017_Maths_and_science_tables_13_and_14_updated.xlsm",
  sheet = "Table 13c",  # worksheet title
  skip = 6,  # column names in eighth row
  na = c("", "x", "NE", "NEW")  # NA values
) 

# Wrangle data

stem_la <- stem_la_raw %>%
  janitor::clean_names() %>%  # make column names sensible
  dplyr::select(
    la_name = region_local_authority_number,
    math = mathematics,
    fmath = further_mathematics,
    biol = biological_sciences,
    chem = chemistry,
    phys = physics,
    comp = computing
    ) %>%
  dplyr::filter(!is.na(la_name)) %>%  # remove rows without LA name
  tidyr::gather(key = subject, value = entry_la, math:comp)


# Join and arrange --------------------------------------------------------


# Join la to sch data

stem_sch_la <- dplyr::left_join(
  stem_sch,
  stem_la,
  by = c("la_name", "subject")
)

# Arrange rowbind

stem_sch_tobind <- stem_sch %>%
  dplyr::select(urn, sch_name, region, la_name, subject, entry = entry_sch) %>%
  dplyr::mutate(geo_level = "sch")

stem_la_tobind <- stem_sch_la %>%
  dplyr::select(urn, sch_name, region, la_name, subject, entry = entry_la) %>%
  dplyr::mutate(geo_level = "la")

stem_sch_la_bind <- bind_rows(stem_sch_tobind, stem_la_tobind)


# Trelliscope -------------------------------------------------------------


# Example subset with no national or la means

stem_sample <- dplyr::filter(
  stem_sch,
  la_name %in% c("East Sussex", "North Somerset", "Westminster")  # random
)

# Nest sample data by school

by_school <- stem_sample %>%
  group_by(sch_name, la_name) %>%
  nest() %>% 
  mutate(
    panel = map_plot(
      data,
      ~ figure(
        ylim = c(0, 100),
        xlab = NULL,
        ylab = NULL,
        tools = NULL
      ) %>%
        ly_points(subject, entry_sch, data = .x, hover = .x)
    )
  )

# Plot

by_school %>%
  trelliscope(
    name = "A-level STEM entry",
    desc = "Percentage entry to STEM A-levels by school 2015/16",
    nrow = 2,
    ncol = 4,
    self_contained = TRUE
  )
