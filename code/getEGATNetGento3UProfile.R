library(rvest) #for web scraping
library(tidyverse)
library(xml2) #work with xml file
library(glue)



year <- 2023
i1 <- list()
url <- list()
loadProfile <- list()
dataProfile <- list()

for(i in 1:2) {
  i1 <- sprintf('%02d', i)
  url <- paste0("http://control.egat.co.th/GetNetGen/default.aspx?d=",i1,"/",year)
  xpath <- "/html/body/form"
  loadProfile[[i]] <-
    url %>% 
    read_html() %>% 
    html_element(xpath = xpath) %>% 
    html_table() %>% 
    rename_all(tolower) %>% 
    mutate(timestamp = as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
           day = day(timestamp),
           hour = hour(timestamp),
           minute = minute(timestamp)) %>% 
    mutate(mac_vspp = mac_v_dummy + mac_v_actual,
           r1_vspp = cac_v_dummy + cac_v_actual,
           r2_vspp = nec_v_dummy + nec_v_actual,
           r3_vspp = sac_v_dummy + sac_v_actual,
           r4_vspp = nac_v_dummy + nac_v_actual,
           .after = edc_exp) %>%
    mutate(cac_edc = cac + edc_exp,
           nec_edl = nec + edl_exp,
           sac_tnb = sac + tnb_exp,
           .after = edc_exp) %>% 
    mutate(egat_net_gen = mac + cac_edc + nec_edl + sac_tnb + nac,
           .after = sac_tnb) %>% 
    mutate(mac_3u = mac + mac_vspp,
           cac_3u = cac_edc + r1_vspp,
           nec_3u = nec_edl + r2_vspp,
           sac_3u = sac_tnb + r3_vspp,
           nac_3u = nac + r4_vspp,
           .after = r4_vspp) %>% 
    mutate(tot_3u = mac_3u + cac_3u + nec_3u + sac_3u + nac_3u,
           .after = nac_3u)
  
  # writexl::write_xlsx(loadProfile[[i]], path = paste0("processdata/",i,"_",year,".xlsx"))
  
}

temp <- as_tibble(loadProfile[[1]]) %>% select(timestamp:mac)  %>% 
  mutate(date = as.Date(timestamp),
         hour = hour(timestamp),
         minute = minute(timestamp),
         time = format(timestamp, format = "%H:%M:%S"),.before = mac) 

# temp2 <- temp %>%
#   mutate(max = rollapply(mac, 
#                          3,
#                          max,
#                          fill = NA,
#                          na.rm = TRUE,
#                          partial = TRUE))

temp3 <-
  temp %>% head(50)

temp4 <-
  temp3 %>% group_by(date, time) %>% 
  mutate(check = case_when(minute >=0 & minute <= 15 ~ "max1",
                           minute >= 15 & minute <= 30 ~ "max2",
                           minute >= 30 & minute <= 45 ~ "max3",
                           minute > 45 ~ "max4"))
                           # minute >= 30 & minute <= 59 ~ "max2"))


dataProfile <- 
# Export load curve to excel files
writexl::write_xlsx(
  loadProfile %>% 
    reduce(full_join, by = c(colnames(.))),
  path = paste0("output_data/loadProfile","_",year,".xlsx")
)
