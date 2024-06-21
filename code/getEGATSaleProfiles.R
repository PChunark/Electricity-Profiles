library(rvest) #for web scraping
library(tidyverse)
library(xml2) #work with xml file
library(glue)
library(readxl)
library(openxlsx)
library(TAF) #for divide all value in column with a constant number
library(scales)


# getEgatSaleProfile <- function(path, pattern){
  
  file_path <- tibble(filename = list.files("raw_data/EGATSaleProfiles",
                                            pattern = "*2023",
                                            full.names = F)) %>% 
    mutate(filepath = paste0("raw_data/EGATSaleProfiles/", filename)) %>% 
    separate(col = filename, into = c("name1","file"), sep = "_") %>% 
    separate(col = file, into = c("month","filetype"), sep = " ") %>% 
    mutate(month = factor(month, levels = month.name)) %>% 
    arrange(month) %>% 
    # select(filepath) %>% 
    pull(filepath)

  colNameProfile <- c("TIME_LOCAL",
                      "MEA", #Egat sale profile MEA
                      "PEA.R1","PEA.R2","PEA.R3","PEA.R4","TOT.PEA", #Egat sale PEA
                      "DIRECT", "DIR.R1","DIR.R3","DIR.R4", #Egat direct customer
                      "FOREIGN","FRG.R1","FRG.R2","FRG.R3",          #Egat foreign direct customer
                      "RESERVE", "RES.R1", "RES.R3","RES.R4",        #Egat reserve direct customer
                      "OTHER",                                       #Egat others (only in R4)
                      "TOT.DIR", "TOT.DIR.R1","TOT.DIR.R2","TOT.DIR.R3","TOT.DIR.R4", #Egat Total direct customer
                      "EGAT.SLE.R1","EGAT.SLE.R2","EGAT.SLE.R3","EGAT.SLE.R4","TOT.EGAT.SLE" #Total Egat sale by region (egat sale + direct customer)
                      )              #dummy column names
  
  
    
  data1 <-
    file_path %>%
    map(function(path){
      read_excel(path, 
                 sheet = 2,
                 skip = 1,
                 # range = cell_cols("A:AE"),
                 col_names = T,
                 .name_repair = "minimal")[,c(1:30)]  #Choose column 1 to 30 
    
    } %>% drop_na()
    )
  
  data1 <- lapply(data1,setNames,colNameProfile) # Rename column name in list using lapply function in base R package
  
  data2 <- data1 %>% 
          map(function(data){
            data %>% 
              select(c("TIME_LOCAL":"TOT.PEA", 
                              "TOT.DIR.R1":"TOT.DIR.R4",
                              "EGAT.SLE.R1":"TOT.EGAT.SLE"))%>%
              mutate(TOT.DIR = TOT.DIR.R1 + TOT.DIR.R2 + TOT.DIR.R3 + TOT.DIR.R4, .after = TOT.DIR.R4)
          }) %>% 
    reduce(full_join, by = c(colnames(.))) %>%  # merge dataframes in a list to a single dataframe
    TAF::div(2:17, by = 1000) %>%  #divide value in all column by 1000
    mutate_if(is.numeric, round, digits = 3) 
  
# }


# a <- getEgatSaleProfile("raw_data/EGATSaleProfiles", "*2023")

  map(data1,sle)