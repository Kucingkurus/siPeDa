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
               ## box(title = strong("Dashboard Penindakan dan Penyidikan 2022"), status = "info", strong("kucingkurusmandi"), p("di papan"), width = 12),
                ### laporan Bongkar
                box(title = strong("Laporan Bongkar"), status = "info", width = 12),
                box(title =  "BA Bongkaran", status = "info", width = 12, plotlyOutput("volumeBongkaran")),
                box(title = "keterangan", status = "info", width = 12, strong("asal"),plotlyOutput("render_jml_bkr_asal")),
                
                ##box(title = "datarange tanggal tes", status = "info", width = 3, dateRangeInput("dateRange1", "tanggal:",
                ##                                                                              start = Sys.Date(),
                ##                                                                              end = Sys.Date(),
                ##                                                                              min = "2022-01-01",
                ##                                                                              max = Sys.Date(),
                ##                                                                              format = "yyyy-mm-dd",
                ##                                                                              separator = "-")),
                
                ### Laporan SBP
                box(title = strong("Jumlah SBP"), status = "info", width = 12),
                box(title = "SBP 2022", status = "primary", width = 6, plotlyOutput("negaraAsalSBP2022")),
                box(title = "SBP by kategori", status = "primary", width = 6, plotlyOutput("kategoriSBP2022")),
                box(title = "jumlah SBP", status = "info", width = 3, strong("NON NPP"), valueBoxOutput("jml_sbp_non"), strong("NPP"), valueBoxOutput("jml_sbp_npp")),
                
                ### laporan pemeriksaan
                box(title = strong("Laporan Pemeriksaan"), status = "info", width = 12),
                box(title = "penjaluran", status = "success", width = 6, plotlyOutput("progresPenjaluran")),
                ### laporan pengajuan LAb
                box(title = strong("Laporan Pengajuan lab"), status = "info", width = 12)
                
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
  
  sbp2022 <- read.csv2("databases/SBP_2022.csv") %>%
    clean_names()
  
  penjaluran <- read.csv2("databases/jumlah_penjaluran.csv") %>%
    clean_names()
  
  dataBongkar <- read.csv2("databases/ba_bongkar.csv") %>%
    clean_names() %>%
    mutate(tanggal_ba_buka_segel = as.Date(tanggal_ba_buka_segel)) %>%
    mutate(berat = as.integer(berat))
  
  dataTangkapan <- read.csv2("databases/data_tangkapan.csv") %>%
    clean_names()

  #panggil data sbp untuk menjadi tabel 
  
  output$tabelSBP2022 <- renderDataTable({sbp2022})
  
  ##PIE CHART SBP BY NEGARA ASAL
  
  ### mengambil data untuk pie-chart negara asal
  negaraAsal <- sbp2022 %>%
    group_by(negara_asal) %>%
    count() %>%
    drop_na()
  
  ### membuat tabel pie chart menggunakan plot_ly
  
  output$negaraAsalSBP2022 <- renderPlotly({
    
    plot_ly(negaraAsal, labels = ~ negara_asal, values = ~n, type = "pie") %>%
      layout(title = 'Negara asal paket yang ditegah',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ##PIE CHART SBP BY KATEGORI
  ###MENGAMBIL DATA SBP BY KATEGORI
  kategoriSBP <- sbp2022 %>%
    group_by(kategori) %>%
    count() %>%
    drop_na()
  
  ###MEMBUAT TABEL PIE CHART BY KATEGORI
  output$kategoriSBP2022 <- renderPlotly({
    
    plot_ly(kategoriSBP, labels = ~ kategori, values = ~n, type = "pie") %>%
      layout(title = 'Kategori paket yang ditegah',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  })
  #line chart penjaluran
  
  output$progresPenjaluran <- renderPlotly({
    plot_ly(penjaluran, x = ~ tanggal) %>%
      add_trace(y= ~total_hijau, name = 'jumlah cn', type = 'scatter', mode = 'lines', line = list(color = 'rgb(124, 252, 0)'))
  })
  
  # value total SBP
  
  jml_sbp_non_val <- sbp2022 %>% 
    drop_na(no_kiriman) %>%
    nrow() %>%
    as.numeric()
  
  jml_sbp_npp_val <- dataTangkapan %>%
    drop_na(no_sbp) %>%
    nrow() %>%
    as.numeric()
  
  output$jml_sbp_non <- renderValueBox({
    valueBox(
      jml_sbp_non_val
    )
  })
  
  output$jml_sbp_npp <- renderValueBox({
    valueBox(
      jml_sbp_npp_val
    )
  })
  
  # laporan bongkar
  
  ##line chart bongkaran
  
  volumeBongkar <-  aggregate(list(jumlahKoli=dataBongkar$total, berat=dataBongkar$berat), by=dataBongkar["tanggal_ba_buka_segel"], sum)
  
  output$volumeBongkaran <- renderPlotly({
    plot_ly(volumeBongkar, x = ~ tanggal_ba_buka_segel) %>%
      add_trace(y= ~jumlahKoli, name = 'Jumlah Koli', type = 'scatter', mode = 'lines', line = list(color = 'rgb(124, 252, 0)')) %>%
      add_trace(y= ~berat, name = 'Berat Paket (Kg)', type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)')) %>%
      layout(title='Berita Acara Bongkar',
             xaxis = list(title = "Tanggal", rangeslider = list(visible = T),
                          rangeselector=list(
                            buttons=list(
                              list(count=1, label="1m", step="month", stepmode="backward"),
                              list(count=6, label="6m", step="month", stepmode="backward"),
                              list(count=1, label="YTD", step="year", stepmode="todate"),
                              list(count=1, label="1y", step="year", stepmode="backward"),
                              list(step="all")
                            ))),
             yaxis = list(title = "Volume"))
  })
  
  
  ## grafik jumlah asal  truk
  
  jml_bkr_asal <- dataBongkar %>%
    count(asal, tanggal_ba_buka_segel) %>%
    pivot_wider(names_from = asal, values_from = n) %>%
    arrange(tanggal_ba_buka_segel)
  
  jml_bkr_asal[is.na(jml_bkr_asal)] = 0
  
  output$render_jml_bkr_asal <- renderPlotly({
    plot_ly(jml_bkr_asal, x = ~ tanggal_ba_buka_segel) %>%
      add_trace(y= ~` SH `, name = 'Soekarno-Hatta', type = 'scatter', mode = 'lines', line = list(color = 'rgb(0, 0, 255)')) %>%
      add_trace(y= ~` PRIOK `, name = 'Tanjung Priok', type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)')) %>%
      add_trace(y= ~` SH (ECOMMERCE) `, name = 'E-Commerce', type = 'scatter', mode = 'lines', line = list(color = 'rgb(0, 255, 0)')) %>%
      layout(title='Berita Acara Bongkar',
             xaxis = list(title = "Tanggal", rangeslider = list(visible = T),
                          rangeselector=list(
                            buttons=list(
                              list(count=1, label="1m", step="month", stepmode="backward"),
                              list(count=6, label="6m", step="month", stepmode="backward"),
                              list(count=1, label="YTD", step="year", stepmode="todate"),
                              list(count=1, label="1y", step="year", stepmode="backward"),
                              list(step="all")
                            ))),
             yaxis = list(title = "Jumlah Truk"))
  })
  
  
}


shinyApp(ui, server)