library(shiny)
library(shinydashboard)
library(readr)
library(magrittr)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(tidyverse)
library(janitor)
library(RColorBrewer)
library(flexdashboard)
library(cssTools)
library(datasets)
library(scales)
library(shinydashboardPlus)
library(fresh)
library(DT)
library(shinyWidgets)
library(ggnewscale)
library(RMySQL)
library(DBI)
#library(RODBC)



killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
    
}

sqlQuery <- function (query) {
  
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(), user="youruser", password='yourpassword', dbname='yourdb', host='192.168.178.1')
  
  # close db connection after function call exits
  on.exit(dbDisconnect(DB))
  
  # send Query to btain result set
  rs <- dbSendQuery(DB, query)
  
  # get elements from result sets and convert to dataframe
  result <- fetch(rs, -1)
  
  # return the dataframe
  return(result)
}

con = dbConnect(RMySQL::MySQL(),
                dbname='P2_pasbar',
                host='103.187.146.198',
                port=3306,
                user='kucingkurus',
                password='Kuc1ngkuru5')

sbp2022 <- dbReadTable(con, "SBP_2022")

#ambil data penjaluran
dataPenjaluran <- dbReadTable(con, "Penjaluran") 

##olah data penjaluran 
penjaluran <- dataPenjaluran %>%
  mutate(Tanggal = as.Date(Tanggal)) %>%
  group_by(Jalur,Tanggal) %>%
  count() %>%
  drop_na()

penjaluran_bulanan <- penjaluran %>%
  group_by(bulan = lubridate::floor_date(Tanggal, 'month'), Jalur) %>%
  summarize(jumlah_jalur = max(n))

penjaluran_bulanan_widen <- penjaluran_bulanan %>%
  pivot_wider(names_from = Jalur, values_from = jumlah_jalur) %>%
  arrange(bulan)

penjaluran_widen <- penjaluran %>%
  pivot_wider(names_from = Jalur, values_from = n) %>%
  arrange(Tanggal)

#jml_bkr_asal_raw <- dataBongkar %>%
#  count(asal, tgl_ba) %>%
 # pivot_wider(names_from = asal, values_from = n) %>%
  #arrange(tgl_ba)

#jml_bkr_asal_raw[is.na(jml_bkr_asal_raw)] = 0



#rekayasa data bongkar dari 2 tabel berbeda menggunakan dplyr

dataBongkar <- dbReadTable(con, "bongkar") %>%
  mutate(tgl_ba = as.Date(tgl_ba))

dataBongkar_r7 <- dbReadTable(con, "bongkar_r7")

dataBongkar_gab <- dataBongkar %>%
  inner_join(dataBongkar_r7, by = c("no_ba"))

agregat_koli <- aggregate(list(jumlahkoli=dataBongkar_gab$koli), by=dataBongkar_gab["tgl_ba"], sum)
agregat_berat <- aggregate(list(berat=dataBongkar$berat), by=dataBongkar["tgl_ba"], sum)

volumeBongkar <- agregat_koli %>%
  inner_join(agregat_berat, by = c("tgl_ba"))

##volumebongkar <- aggregate(list(jumlahkoli=dataBongkar_gab$koli, berat=dataBongkar_gab$berat), by=dataBongkar_gab["tgl_ba"], sum)

## ambil data penjaluran



negaraAsal <- sbp2022 %>%
  group_by(negara_asal) %>%
  count() %>%
  drop_na()

jml_sbp_npp <- dataTangkapan %>%
  drop_na(no_sbp) %>%
  nrow() %>%
  as.numeric()

x <- c(1:100)
random_y <- rnorm(100, mean = 0)
data <- data.frame(x, random_y)


##jumlah asal  truk

jml_bkr_asal_raw <- dataBongkar %>%
  count(asal, tgl_ba) %>%
  pivot_wider(names_from = asal, values_from = n) %>%
  arrange(tgl_ba)
  
  jml_bkr_asal_raw[is.na(jml_bkr_asal_raw)] = 0

