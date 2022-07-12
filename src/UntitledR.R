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


sbp2022 <- read_csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/SBP_2022.csv") %>%
  clean_names()

negaraAsal <- sbp2022 %>%
  group_by(negara_asal) %>%
  count() %>%
  drop_na()

penjaluran <- read.csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/jumlah_penjaluran.csv") %>%
  clean_names()

dataBongkar <- read.csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/ba_bongkar.csv") %>%
  clean_names()

volumebongkar <- aggregate(dataBongkar["total"], by=dataBongkar["tanggal_ba_buka_segel"], sum)

x <- c(1:100)
random_y <- rnorm(100, mean = 0)
data <- data.frame(x, random_y)

