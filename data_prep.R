library(dplyr)
library(readr)

#Prescribing data for Scotland
#from https://www.opendata.nhs.scot/dataset/prescriptions-in-the-community
#downloaded most recent 12 months
rx <-
  list.files(pattern = "^pit") %>% 
  purrr::map_df(~data.table::fread(., 
                                   sep = ",",
                                   na.strings = 
                                     c(NA_character_, ""))) %>%
  as_tibble() %>%
  janitor::clean_names()

#BNF file
#https://applications.nhsbsa.nhs.uk/infosystems/data/showDataSelector.do?reportId=126

#BNF data 
#recode dummy glucose monitors
bnf <-
  readr::read_csv("20220905_1662367590394_BNF_Code_Information.csv") %>%
  janitor::clean_names() %>%
  mutate(bnf_chemical_substance = 
           ifelse(bnf_chemical_substance == "DUMMY CHEMICAL SUBSTANCE 214800001",
                  "Glucose monitor", bnf_chemical_substance)) %>%
  write_csv("bnf.csv")

#Subset just chemicals and section
bnf_chemical_substance <-
  bnf %>%
  select(bnf_chemical_substance, bnf_section) %>%
  distinct() %>%
  group_by(bnf_chemical_substance) %>%
  slice_head() %>%
  write_csv("bnf_chemical_substance.csv")

#GP Practice data for July 2022
#https://www.opendata.nhs.scot/dataset/gp-practice-contact-details-and-list-sizes

grampian_gps <- 
  read_csv("aggregated_grampian_gp_codes.csv") 

#add BNF data to prescribing
rx <-
  left_join(rx, bnf, by = c("bnf_item_code" = "bnf_presentation_code"))


#keep Grampian data only (code of S08000020)
#exclude prescriptions that came from the hospital
grampian <-
  rx %>%
  filter(hbt == "S08000020",
         gp_practice != 99998)

#add GP data to prescribing  
grampian <-
  left_join(grampian, grampian_gps, by = c("gp_practice" = "practice_code"))

#Write Grampian data to file
readr::write_csv(grampian, "grampian_gp_rx.csv")

#instead of sums per GP, make table for sum rx across all practices
grampian_rx <-
  grampian %>%
  group_by(bnf_item_code) %>%
  mutate(
    total_cost = sum(gross_ingredient_cost),
    total_rx = sum(number_of_paid_items),
    cost_rx =
      total_cost /
      total_rx
  ) %>%
  ungroup() %>%
  group_by(bnf_item_code) %>%
  slice_head() %>%
  select(-c(hbt, gp_practice, number_of_paid_items, paid_quantity, gross_ingredient_cost, paid_date_month, gp_practice_name)) %>%
  write_csv("grampian_rx.csv")
