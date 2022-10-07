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
library(RODBC)



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

dataBongkar <- dbReadTable(con, "ba_bongkar") %>%
  clean_names() %>%
  mutate(tanggal_ba_buka_segel = as.Date(tanggal_ba_buka_segel)) %>%
  mutate(x_berat = as.numeric(x_berat)) %>%
  mutate(total = as.integer(total))

volumebongkar <- aggregate(list(jumlahkoli=dataBongkar$total, berat=dataBongkar$x_berat), by=dataBongkar["tanggal_ba_buka_segel"], sum)


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
  count(asal, tanggal_ba_buka_segel) %>%
  pivot_wider(names_from = asal, values_from = n) %>%
  arrange(tanggal_ba_buka_segel)
  
  jml_bkr_asal_raw[is.na(jml_bkr_asal_raw)] = 0

