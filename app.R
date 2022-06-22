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
library(shiny)
library(shinydashboard)
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
      menuItem("Penjaluran", tabName = "penjaluran", icon = icon("code-branch")),
      menuItem("Bongkaran", tabName = "bongkaran",icon = icon("box-open"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboardP2",
              fluidRow(
                box(title = strong("Dashboard consignment Note 2022"), status = "info", strong("kucingkurusmandi"), p("di papan"), width = 12)
              )),
      tabItem(tabName = "tabelSBP2021",
            fluidRow(
              box(title = strong("Dashboard SBP 2021"), status = "info", width = 12),
              box(title = "Dashboard", width = 12, plotlyOutput("dbsbp2021")),
              box(title = "Tabel", width =12, dataTableOutput("tabelsbp2021"))
              )
            )
    )
  )
)


server <- function(input, output, session) {
 #load data
  #sbp
  sbp2021 <- read_csv2("D:/OneDrive/Pribadi/Benkyou/data science/Dashboard_Project/siPeDa/Databases/sbp_2021.csv")
  
  #panggil data esbepe
  
  output$tabelsbp2021 <- renderDataTable({sbp2021})
}

shinyApp(ui, server)