library(dplyr)

#BNF file dowloaded from
#https://applications.nhsbsa.nhs.uk/infosystems/data/showDataSelector.do?reportId=126

bnf <-
  readr::read_csv("20220905_1662367590394_BNF_Code_Information.csv") %>%
  janitor::clean_names() %>%
  mutate(bnf_product = 
           ifelse(bnf_product == "DUMMY PRODUCT 21480000101",
                  "FreeStyle Libre 2", bnf_product),
         bnf_product = 
           ifelse(bnf_product == "DUMMY PRODUCT 21480000100",
                  "FreeStyle Libre", bnf_product)) #FreeStyles coded as DUMMY fixed

#Prescribing data for all Scotland
#from https://www.opendata.nhs.scot/dataset/prescriptions-in-the-community
#downloaded most recent 12 months
rx <-
  list.files(pattern = "^pit") %>% 
  purrr::map_df(~data.table::fread(., 
                                   sep = ",",
                                   na.strings = 
                                     c(NA_character_, ""))) %>%
  as_tibble() 


#keep Grampian data only (code of S08000020)
#exclude prescriptions that came from the hospital
grampian <-
  rx %>%
  filter(HBT == "S08000020",
         GPPractice != 99998)

#add BNF data to prescribing tables
grampian <-
  left_join(grampian, bnf, by = c("BNFItemCode" = "bnf_presentation_code"))


#Write Grampian data to file
readr::write_csv(grampian, "grampian_rx.csv")

boxplot(cars$speed)
