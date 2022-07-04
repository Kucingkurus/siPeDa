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

ui <- dashboardPage(
  title = "Kucing Kurus",
  dashboardHeader(
    title = "Kucing Kurus"
  ),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
    menuItem("Dashboard P2", tabName = "dashboardP2", icon = icon("archway",lib = "font-awesome")),
    menuItem("Detail", tabName = "detail", icon=icon("database"),
      menuItem("SBP 2021", tabName = "tabelSBP2021",icon=icon("users")),
      menuItem("SBP 2022", tabName = "tabelSBP2022", icon = icon("users")),
      menuItem("Penjaluran", tabName = "penjaluran", icon = icon("code-branch")),
      menuItem("Bongkaran", tabName = "bongkaran",icon = icon("box-open"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboardP2",
              fluidRow(
                box(title = strong("Dashboard Penindakan dan Penyidikan 2022"), status = "info", strong("kucingkurusmandi"), p("di papan"), width = 12),
                box(title = "Trend SBP 2022", status = "primary", width = 12, plotlyOutput("trendSBP2022"))
              )),
      tabItem(tabName = "tabelSBP2022",
            fluidRow(
              box(title = strong("Dashboard SBP 2021"), status = "info", width = 12),
              box(title = "Dashboard", width = 12, plotlyOutput("dbsbp2021")),
              box(title = "Tabel", width =12, dataTableOutput("tabelSBP2022"))
              )
            )
    )
  )
)


server <- function(input, output, session) {
 #load data
  #sbp
  ## sbp2021 <- read_csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/sbp_2021.csv")
  sbp2022 <- read.csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/SBP_2022.csv") %>%
    clean_names()
  
  #panggil data esbepe
  
  output$tabelSBP2022 <- renderDataTable({sbp2022})
  
  negaraAsal <- sbp2022 %>%
    group_by(negara_asal) %>%
    count() %>%
    drop_na()
  
  output$trendSBP2022 <- renderPlotly({
    
    ggplot(negaraAsal, aes(x="", y=n, fill=negara_asal)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0)
  })
}

shinyApp(ui, server)