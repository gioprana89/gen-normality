

#library(webr)


#library(plyr)
library(readxl)
#library(openxlsx)
#library(data.table)
library(shiny)
#library(shinyAce)
#source("chooser.R")

#library(lavaan)

#library(mnormt)
#library(curl)
#library(plspm)

library(moments)


########################################
########UI (User Interface)#############
########################################

modul_normal_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
    
    
    fluidRow(
      column(4,
             
             
 

             
             
             
             
             br()
             
      ),
             
             
      column(4,
             
             
  
             
             textAreaInput(ns("jumlah_bilangan_acak"), 
                           h3("Jumlah Bilangan Acak Normal yang Ingin Dibangkitkan:",style="red; text-align:left;font-size:14px"         )
                           
                           ,value="20", height = 50),    
             
             
             
             
             textAreaInput(ns("input_rata_rata"), 
                           h3("Nilai Rata-Rata Bilangan Acak yang Diinginkan:",style="red; text-align:left;font-size:14px"         )
                           
                           ,value="0", height = 50),    
             
             
             textAreaInput(ns("input_standar_deviasi"), 
                           h3("Nilai Standar Deviasi Bilangan Acak yang Diinginkan:",style="red; text-align:left;font-size:14px"         )
                           
                           ,value="100", height = 50),    
             
             
             
             
             
             
             
             
             
             br()
             
      ),
      
      
      
      column(4,
             
             
             br()
             
      )
      
      
      
    ),
             
             
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    fluidRow(
      column(4,
             
             
             
         
             
             
             
             br()
             
      ),
      
      
      column(4,
             
             
             
             
             textAreaInput(ns("input_breaks"), 
                           h3("Breaks:",style="red; text-align:left;font-size:14px"         )
                           
                           ,value="4", height = 50),    
             
             br(),
             
             plotOutput(ns("gambar_histogram")),
             
             
             br(),
             br(),
             
             
             
             
             
             
             h3("Berikut Bilangan Acak Anda",style="red; text-align:center;font-size:30px"         ),
             
             
             
             br(),
             
             
             
             verbatimTextOutput(ns("bilangan_acak_ku")),
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             br()
             
      ),
      
      
      
      column(4,
             
             
             br()
             
      )
      
      
      
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
 
               
               
               
               
    
    
    br()
    
  ) #Akhir Fluidpage
  
  
} #Akhir dari modul_normal_ui

#Akhir dari modul_normal_ui
#Akhir dari modul_normal_ui
#Akhir dari modul_normal_ui
#Akhir dari modul_normal_ui











































































########################################
################Server##################
########################################



modul_normal_server <- function(input, output, session) {
  
  
  
  Kirim_bilangan_acak <- function()
  {
    
    
    jumlah_bilangan_acak <- input$jumlah_bilangan_acak
    jumlah_bilangan_acak <- as.numeric(jumlah_bilangan_acak)
    
    
    input_rata_rata <- input$input_rata_rata
    input_rata_rata <- as.numeric(input_rata_rata)
    
    
    input_standar_deviasi <- input$input_standar_deviasi
    input_standar_deviasi <- as.numeric(input_standar_deviasi)
    
    bil_acak <- rnorm(n = jumlah_bilangan_acak, mean = input_rata_rata, sd = input_standar_deviasi)
    
    
    return(bil_acak)
    
  }
  
  
  
  
  
  ################
  
  
  
  output$gambar_histogram <- renderPlot({
    
    
    bil_acak <- Kirim_bilangan_acak()
    
    
    input_breaks <- input$input_breaks
    input_breaks <- as.numeric(input_breaks)
    
    gambar <- hist(bil_acak, breaks = input_breaks, main = c("Histogram dari Bilangan Acak Normal"))
    
    print(gambar)
    
    
    
  })
  
  
  
  
  
  ###############
  
  
  
  
  output$bilangan_acak_ku <- renderPrint({
    
    
    bil_acak <- Kirim_bilangan_acak()
    take_data <- bil_acak
    
    KS_asymp <- ks.test(take_data,"pnorm", mean(take_data), sd(take_data), exact = TRUE  )
    nilai_pvalue = KS_asymp$p.value

    
    if(nilai_pvalue >= 0.05)
    {
      cat(sprintf("Berdasarkan Hasil Uji Normalitas Kolmogorov-Smirnov, \nData Acak yang Anda Bangkitkan Berdistribusi Normal, \ndengan Nilai Probabilitas = %f >= 0.05\n\n\n", nilai_pvalue))
    }
    if(nilai_pvalue < 0.05)
    {
      cat(sprintf("Berdasarkan Hasil Uji Normalitas Kolmogorov-Smirnov, \nData Acak yang Anda Bangkitkan Tidak Berdistribusi Normal, \ndengan Nilai Probabilitas = %f < 0.05\n\n\n", nilai_pvalue))
    }
    
    
    
    
    SW_asymp <- shapiro.test(take_data)
    nilai_pvalue = SW_asymp$p.value
    
    if(nilai_pvalue >= 0.05)
    {
      cat(sprintf("Berdasarkan Hasil Uji Normalitas Shapiro-Wilk, \nData Acak yang Anda Bangkitkan Berdistribusi Normal, \ndengan Nilai Probabilitas = %f >= 0.05\n\n\n", nilai_pvalue))
    }
    if(nilai_pvalue < 0.05)
    {
      cat(sprintf("Berdasarkan Hasil Uji Normalitas Shapiro-Wilk, \nData Acak yang Anda Bangkitkan Tidak Berdistribusi Normal, \ndengan Nilai Probabilitas = %f < 0.05\n\n\n", nilai_pvalue))
    }
    
    
    
    
    
    
    
    JB_asymp <-  moments::jarque.test(take_data)
    nilai_pvalue = JB_asymp$p.value
    
    
    
    if(nilai_pvalue >= 0.05)
    {
      cat(sprintf("Berdasarkan Hasil Uji Normalitas Jarque-Bera, \nData Acak yang Anda Bangkitkan Berdistribusi Normal, \ndengan Nilai Probabilitas = %f >= 0.05\n\n\n", nilai_pvalue))
    }
    if(nilai_pvalue < 0.05)
    {
      cat(sprintf("Berdasarkan Hasil Uji Normalitas Jarque-Bera, \nData Acak yang Anda Bangkitkan Tidak Berdistribusi Normal, \ndengan Nilai Probabilitas = %f < 0.05\n\n\n", nilai_pvalue))
    }
    
    
    
    
    
cat(sprintf("Berikut Bilangan Acak Normal Anda:\n\n"))
    
    for(i in 1 : length(bil_acak))
    {
      
      cat(sprintf("%f\n", bil_acak[i]))
      
    }
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
} #akhir dari modul_normal_server

#akhir dari modul_normal_server
#akhir dari modul_normal_server
#akhir dari modul_normal_server

















































































ui <- fluidPage(
  
  
  includeHTML("intro_home.html"),
  
  
  uiOutput("modul_normal"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_normal <- renderUI({
    
    
    
    #source("module//modul_normal.R")
    callModule(module = modul_normal_server, id = "modul_normal")
    modul_normal_ui(id = "modul_normal")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














