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
  title = "Dashboard P2",
  dashboardHeader(
    title = "Dashboard P2"
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
      ##dashboard utama
      tabItem(tabName = "dashboardP2",
              fluidRow(
                box(title = strong("Dashboard Penindakan dan Penyidikan 2022"), status = "info", strong("kucingkurusmandi"), p("di papan"), width = 12),
                box(title = "SBP 2022", status = "primary", width = 6, plotlyOutput("negaraAsalSBP2022")),
                box(title = "penjaluran", status = "success", width = 6, plotlyOutput("progresPenjaluran")),
                box(title =  "BA Bongkaran", status = "info", width = 6, plotlyOutput("volumeBongkaran"))
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
  
  ## sbp2021 <- read_csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/sbp_2021.csv")
  
  sbp2022 <- read.csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/SBP_2022.csv") %>%
    clean_names()
  
  penjaluran <- read.csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/jumlah_penjaluran.csv") %>%
    clean_names()
  
  dataBongkar <- read.csv2("/home/kucingkurus/Documents/Web Development/siPeDa/Databases/ba_bongkar.csv") %>%
    clean_names()
  
  #panggil data sbp untuk menjadi tabel 
  
  output$tabelSBP2022 <- renderDataTable({sbp2022})
  
  ## mengambil data untuk pie-chart negara asal
  negaraAsal <- sbp2022 %>%
    group_by(negara_asal) %>%
    count() %>%
    drop_na()
  
  ## membuat tabel pie chart menggunakan plot_ly
  
  output$negaraAsalSBP2022 <- renderPlotly({
    
    plot_ly(negaraAsal, labels = ~ negara_asal, values = ~n, type = "pie") %>%
      layout(title = 'Negara asal paket yang ditegah',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #line chart penjaluran
  
  output$progresPenjaluran <- renderPlotly({
    plot_ly(penjaluran, x = ~ tanggal) %>%
      add_trace(y= ~total_hijau, name = 'jumlah cn', type = 'scatter', mode = 'lines', line = list(color = 'rgb(124, 252, 0)'))
  })
  
  #line chart bongkaran
  
  volumeBongkar <- aggregate(dataBongkar["total"], by=dataBongkar["tanggal_ba_buka_segel"], sum)
  
  output$volumeBongkaran <- renderPlotly({
    plot_ly(volumeBongkar, x = ~ tanggal_ba_buka_segel) %>%
      add_trace(y= ~total, name = 'Volume Bongkaran', type = 'scatter', mode = 'lines', line = list(color = 'rgb(124, 252, 0)'))
  })
}

shinyApp(ui, server)