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

trend <- sbp2022 %>%
  group_by(negara_asal) %>%
  count() %>%
  drop_na()
