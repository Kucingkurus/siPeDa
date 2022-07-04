library(shiny)
library(shinydashboard)
library(readr)
library(magrittr)
library(readxl)
library(ggplot2)
library(dplyr)
library (lubridate)
library(plotly)
library(tidyverse)
library(janitor)
library (RColorBrewer)
library(flexdashboard)
library(shiny)
library(shinydashboard)
library(cssTools)
library(datasets)
library (scales)
library(fresh)
library(DT)
library(shinyWidgets)
library(ggnewscale)
ui <- dashboardPage(
  dashboardHeader(
    title = "Bismillah SI MAS"
    
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
    menuItem("Tentang", tabName = "tentang", icon=icon("clipboard")),
    menuItem("Dashboard CN", tabName = "cn", icon=icon("database"),
             menuItem("Rekapitulasi 2021", tabName="cn2021", icon=icon("gears")),
             menuItem("Rekapitulasi 2022", tabName="cn2022", icon=icon("gears"),
                      menuSubItem("pembetulan", tabName="pb", icon=icon("gears")))
             )
    )
    
  )
  ,
  dashboardBody(
    tabItems(
      tabItem(tabName = "tentang",
        fluidRow(
          box(title = strong("SELAMAT DATANG!"), width = 12))
      ),
      tabItem(tabName = "pb",
              fluidRow(
                box(title = "Dashboard dan Data Pembetulan 2021", status = "info", 
                    strong("Berikut untuk Dashboard dokumen pembetulan:"), width = 12),
                tabBox(title = strong("LAYANAN CN"),  
                       width = 12,
                       tabPanel("Bulanan",plotlyOutput("dbpembetulan2021")),
                       tabPanel("Tahunan",plotlyOutput("db2pembetulan2021"))),
                box(title = strong("Tabel"), width = 12,dataTableOutput("tbpembetulan2021"))
              )
      )
    )
  )
)
        
      


server <- function(input, output, session) {
  
  # LOAD DATA
  # Pembetulan
  pembetulan2021 <- read_csv2("D:/OneDrive/Pribadi/Benkyou/data science/Dashboard_Project/siPeDa/Databases/sbp_2021.csv") %>%
    clean_names()
  
  #Tabel Pembetulan
  output$tbpembetulan2021 <- renderDataTable ({pembetulan2021 %>% 
      select (nama_importir,bulan_proses,no_cn,tgl_cn)})
  
  output$dbpembetulan2021 <- renderPlotly({
    total <- pembetulan2021 %>% 
      select (tgl_surat, bulan_proses) %>%
      mutate(tgl_surat = dmy(`tgl_surat`)) %>% 
      arrange((tgl_surat))
    
    data <- setNames(data.frame(table(total$bulan_proses)),c("bulan_proses","jumlah_cn")) %>%
      mutate(bulan_proses = dmy(`bulan_proses`)) %>% 
      arrange((bulan_proses)) %>%
      
      ggplot(aes(x = `bulan_proses`, y = `jumlah_cn`, fill = `bulan_proses`)) +
      geom_bar(width = 25, stat = "identity",color = "#000000") + 
      geom_text(aes(x= `bulan_proses`, y = `jumlah_cn`, label= `jumlah_cn`), color = "white", 
                position = position_stack(vjust = 0.5)) + labs ()})
  
  
  
  
  
}

shinyApp(ui, server)