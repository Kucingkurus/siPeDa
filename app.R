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

ui <- dashboardPage(
  title = "Dashboard P2",
  dashboardHeader(
    title = "Dashboard P2"
  ),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
    menuItem("Dashboard P2", tabName = "dashboardP2", icon = icon("archway",lib = "font-awesome")),
    menuItem("Detail", tabName = "detail", icon=icon("database"),
      menuItem("kucingkurus", icon=icon("users"), href = "http://www.p2pasbar.store/siPeDa/src/"),
      menuItem("SBP 2022", tabName = "tabelSBP2022", icon = icon("users")),
     #menuItem("Penjaluran", tabName = "penjaluran", icon = icon("code-branch")),
      menuItem("Bongkaran", tabName = "bongkaran", icon = icon("box-open"))
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
                
                ###laporan pemeriksaan
                box(title = strong("Laporan Pemeriksaan"), status = "info", width = 12),
                box(title = "penjaluran", status = "success", width = 12, plotlyOutput("penjaluran_bulanan")),
                
                ### Laporan SBP
                box(title = strong("Laporan SBP"), status = "info", width = 12),
                box(title = "SBP 2022", status = "primary", width = 6, plotlyOutput("negaraAsalSBP2022")),
                box(title = "SBP by kategori", status = "primary", width = 6, plotlyOutput("kategoriSBP2022")),
                box(title = "jumlah SBP", status = "info", width = 6, strong("NON NPP"), valueBoxOutput("jml_sbp_non"), strong("NPP"), valueBoxOutput("jml_sbp_npp")),
                

                ### laporan pengajuan Lab
                #box(title = strong("Laporan Pengajuan lab"), status = "info", width = 12)
                
              )),
      
      tabItem(tabName = "tabelSBP2022",
            fluidRow(
              #box(title = strong("Dashboard SBP 2021"), status = "info", width = 12),
              #box(title = "Dashboard", width = 12, plotlyOutput("dbsbp2021")),
              box(title = "Tabel", width =12, dataTableOutput("tabelSBP2022"))
              )
            ),
      
      tabItem(tabName = "bongkaran",
               fluidRow(
                 box(title = "tabel bongkaran", width= 12, dataTableOutput(("tabelBongkaran")))
               ))
    )
  )
)


server <- function(input, output, session) {
 #load data
  
  ## Load data SQL
  
  ### login data sql
  
  con = dbConnect(RMySQL::MySQL(),
                  dbname='P2_pasbar',
                  host='103.187.146.198',
                  port=3306,
                  user='kucingkurus',
                  password='Kuc1ngkuru5')
  
  ### load data sbp dari server sql
  sbp2022 <- dbReadTable(con, "sbp_2022") %>%
    clean_names()
  kodenegara <- dbReadTable(con, "sbp_kodenegara")
  detail_paket <- dbReadTable(con,  "detail_paket")
  detail_sbp <- dbReadTable(con, "sbp_detail")
  sbp_kategori <- dbReadTable(con, "sbp_kategori")
  
  #ambil data penjaluran
  dataPenjaluran <- dbReadTable(con, "Penjaluran") 
  
  ### laod data bongkaran dari server sql
  ####rekayasa data bongkar dari 2 tabel berbeda menggunakan dplyr
  
  dataBongkar <- dbReadTable(con, "bongkar") %>%
    mutate(tgl_ba = as.Date(tgl_ba))
  
  dataBongkar_r7 <- dbReadTable(con, "bongkar_r7")
  
  dataBongkar_gab <- dataBongkar %>%
    inner_join(dataBongkar_r7, by = c("no_ba"))
  
  
  ##Load data CSV
  
 # sbp2022 <- read.csv2("src/databases/SBP_2022.csv") %>%
   # clean_names()
  
  penjaluran <- read.csv2("src/databases/jumlah_penjaluran.csv") %>%
    clean_names()
  
#dataBongkar <- read.csv2("src/databases/ba_bongkar.csv") %>%
 #   clean_names() %>%
  #  mutate(tanggal_ba_buka_segel = as.Date(tanggal_ba_buka_segel)) %>%
   # mutate(berat = as.integer(berat))
  
  dataTangkapan <- read.csv2("src/databases/data_tangkapan.csv") %>%
    clean_names()
  
  #panggil data sbp untuk menjadi tabel 
  
  output$tabelSBP2022 <- renderDataTable({sbp2022})
  
  #panggil data bongkaran untuk menjadi tabel
  
  output$tabelBongkaran <- renderDataTable({dataBongkar})
  
  ##PIE CHART SBP BY NEGARA ASAL
  
  ### mengambil data untuk pie-chart negara asal
  sbp_join_detail_negara<- detail_paket %>%
    left_join(kodenegara, by = c("kode_negara"))
  
  
  negaraAsal <- sbp_join_detail_negara %>%
    group_by(negara) %>%
    count() %>%
    drop_na()
  
  ### membuat tabel pie chart menggunakan plot_ly
  
  output$negaraAsalSBP2022 <- renderPlotly({
    
    plot_ly(negaraAsal, textinfo = "none", labels = ~ negara, values = ~n, type = "pie") %>%
      layout(title = 'Negara asal paket yang ditegah',
             showlegend = FALSE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ##PIE CHART SBP BY KATEGORI
  ###MENGAMBIL DATA SBP BY KATEGORI
  sbp_join_detail_kategori <- detail_sbp %>%
    right_join(sbp_kategori, by = c("komoditi" = "no_komoditi"))
  
  kategoriSBP <- sbp_join_detail_kategori %>%
    group_by(deskripsi_komoditi) %>%
    count() %>%
    drop_na()
  
  ###MEMBUAT TABEL PIE CHART BY KATEGORI
  output$kategoriSBP2022 <- renderPlotly({
    
    plot_ly(kategoriSBP, textinfo = "none", labels = ~ deskripsi_komoditi, values = ~n, type = "pie") %>%
      layout(title = 'Kategori paket yang ditegah',
             showlegend = FALSE,
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
    drop_na(no_dok) %>%
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
  
  agregat_koli <- aggregate(list(jumlahkoli=dataBongkar_gab$koli), by=dataBongkar_gab["tgl_ba"], sum)
  agregat_berat <- aggregate(list(berat=dataBongkar$berat), by=dataBongkar["tgl_ba"], sum)
  
  volumeBongkar <- agregat_koli %>%
    inner_join(agregat_berat, by = c("tgl_ba"))
  
  #
  output$volumeBongkaran <- renderPlotly({
    plot_ly(volumeBongkar, x = ~ tgl_ba) %>%
      add_trace(y= ~jumlahkoli, name = 'Jumlah Koli', type = 'scatter', mode = 'lines', line = list(color = 'rgb(124, 252, 0)')) %>%
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
    count(asal, tgl_ba) %>%
    pivot_wider(names_from = asal, values_from = n) %>%
    arrange(tgl_ba)
  
  jml_bkr_asal[is.na(jml_bkr_asal)] = 0
  
  output$render_jml_bkr_asal <- renderPlotly({
    plot_ly(jml_bkr_asal, x = ~ tgl_ba) %>%
      add_trace(y= ~ SH, name = 'Soekarno-Hatta', type = 'scatter', mode = 'lines', line = list(color = 'rgb(0, 0, 255)')) %>%
      add_trace(y= ~ PRIOK, name = 'Tanjung Priok', type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)')) %>%
      add_trace(y= ~ `E-COMMERCE`, name = 'E-Commerce', type = 'scatter', mode = 'lines', line = list(color = 'rgb(0, 255, 0)')) %>%
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
  
  ##grafik penjaluran
  ###olah data penjaluran 
  penjaluran <- dataPenjaluran %>%
    mutate(Tanggal = as.Date(Tanggal)) %>%
    group_by(Jalur,Tanggal) %>%
    count() %>%
    drop_na()
  
  penjaluran_bulanan <- penjaluran %>%
    group_by(bulan = lubridate::floor_date(Tanggal, 'month'), Jalur) %>%
    summarize(jumlah_jalur = max(n))
  
 # penjaluran_bulanan_widen <- penjaluran_bulanan %>%
   # pivot_wider(names_from = Jalur, values_from = jumlah_jalur) %>%
    #arrange(bulan)
  
  penjaluran_widen <- penjaluran %>%
    pivot_wider(names_from = Jalur, values_from = n) %>%
    arrange(Tanggal)
  
  ### plotly bar chart penjaluran 
  output$penjaluran_bulanan <- renderPlotly({
    plot_ly(penjaluran_widen, x = ~Tanggal, y = ~Hijau, type = 'bar', name = 'Hijau', marker = list(color = 'rgb(44,255,44)')) %>%
      add_trace(y = ~Merah, name = 'Merah', marker = list(color = 'rgb(255,44,44)')) %>%
      layout(xaxis = list(title = "Periode", tickangle = -45 , tickformat="%d %B %Y"),
             yaxis = list(title = "Jalur"),
             margin = list(b = 100),
             barmode = 'group')
    
  })
  
}


shinyApp(ui, server)