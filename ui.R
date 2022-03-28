library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(latticeExtra)
library(dygraphs)
library(xts)
library(lubridate)

shinyUI(dashboardPage(
  dashboardHeader(title = HTML(
    "<div style = 'vertical-align:middle'>
       <img src = 'https://thienmaonline.vn/quan-tieng-anh-la-gi/imager_1_5405_600.jpg' align = 'center' width = '80px' >
       Dashboard
       </div>")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dá»¯ liá»u báº£ng", tabName = "data", icon = icon("table")),
      menuItem("Tá»ng quan", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Pie Chart", tabName = "piechart", icon = icon("chart-pie")),
      menuItem("Line Chart", tabName = "linechart", icon = icon("chart-line")),
      menuItem("Model dá»± ÄoÃ¡n dÃ¢n sá»", tabName = "modelPopulation", icon = icon("uncharted")),
      menuItem("Model dá»± dá»± ÄoÃ¡n GDP", tabName = "modelGDP", icon = icon("uncharted"))
    )),
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "data",
              dataTableOutput("mytable1")
      ),
      # Second tab content
      tabItem(tabName = "dashboard",
              h1("QuÃ¡ trÃ¬nh thay Äá»i cÃ¡c chá» sá» tá»« nÄm 1960 - 2020", align = "center", style = "color:#df622d"),
              sidebarLayout(
                # Sidebar panel for inputs ----
                
                sidebarPanel(
                  # Input: Selector for choosing dataset ----
                  
                  selectInput(inputId = "code",
                              label = "Thay Äá»i dá»¯ liá»u",
                              choices = c(names(data)),
                              selected = names(data)[[2]]), style = "background-color:#657785; color:white"
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  plotlyOutput("myplot")
                )
              )
              
      ),
      
      # 3rd tab content
      tabItem(tabName = "piechart",
              h3("CÆ¡ cáº¥u GDP theo lÄ©nh vá»±c", align = "center", style = "color:#fd5607"),
              sidebarLayout(
                sidebarPanel(
                  sliderInput("sliderYear", label = h3("Thay Äá»i nÄm"), min = 1985, 
                              max = 2020, value = 1985, animate= TRUE, sep = ""),
                  style = "background-color:#657785; color:white"
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  column(width = 9,
                         plotOutput("PieGDP")),
                  
                  column(width = 3,       
                         box(width = 12,
                             title = "% cÃ´ng nghiá»p",
                             solidHeader = TRUE,
                             status = "warning",
                             div(strong(textOutput("congnghiep"))), 
                             style="color:white; background-color: #475e69; font-size: 16px"),
                         box(width = 12,
                             title = "% nÃ´ng nghiá»p",
                             solidHeader = TRUE,
                             status = "info",
                             textOutput("nongnghiep"), 
                             style="color:white; background-color: #475e69; font-size: 16px"),
                         box(width = 12,
                             title = "% dá»ch vá»¥",
                             solidHeader = TRUE,
                             status = "success",
                             textOutput("dichvu"), 
                             style="color:white; background-color: #475e69; font-size: 16px"),
                         box(width = 12,
                             title = "Tá»ng giÃ¡ trá» GDP - Triá»u USD",
                             solidHeader = TRUE,
                             status = "danger",
                             textOutput("totalGDP"), 
                             style="color:white; background-color: #475e69; font-size: 16px"))
                )
              ),
              column(width = 4,
                     h4("CÃ¡c chá» sá» pháº£n Ã¡nh ná»n kinh táº¿", align = "center", style = "color:#0aac4b"),
                     fluidRow(
                       box(width = 6,
                           title = "GDP theo Äáº§u ngÆ°á»i - USD",
                           solidHeader = TRUE,
                           status = "primary",
                           textOutput("peopleGDP"), 
                           style="color:white; background-color: #475e69; height: 50px; font-size: 20px"),
                       box(width = 6,
                           title = "Tá»ng tÃ­ch luá»¹ tÃ i sáº£n %GDP",
                           solidHeader = TRUE,
                           status = "warning",
                           textOutput("tichluyUSD"), 
                           style="color:white; background-color: #475e69; height: 50px; font-size: 20px")
                     ),
                     
                     fluidRow(
                       box(width = 6,
                           title = "GiÃ¡ trá» thÆ°Æ¡ng máº¡i hÃ ng hoÃ¡ %GDP",
                           solidHeader = TRUE,
                           status = "success",
                           textOutput("tmhanghoa"), 
                           style="color:white; background-color: #475e69; height: 50px; font-size: 20px"),
                       box(width = 6,
                           title = "GiÃ¡ trá» Äáº§u tÆ° nÆ°á»c ngoÃ i BoP - Triá»u USD",
                           solidHeader = TRUE,
                           status = "success",
                           textOutput("dautuNN"), 
                           style="color:white; background-color: #475e69; height: 50px; font-size: 20px"),
                     ),
                     
                     fluidRow(
                       box(width = 6,
                           title = "GiÃ¡ trá» láº¡m phÃ¡t áº£nh hÆ°á»ng GDP - %",
                           solidHeader = TRUE,
                           status = "danger",
                           textOutput("lamp"), 
                           style="color:white; background-color: #475e69; height: 50px; font-size: 20px"),
                       box(width = 6,
                           title = "GiÃ¡ trá» láº¡m phÃ¡t giÃ¡ tiÃªu dÃ¹ng - %",
                           solidHeader = TRUE,
                           status = "danger",
                           textOutput("lampgia"), 
                           style="color:white; background-color: #475e69; height: 50px; font-size: 20px"),
                     )
              ),
              column(width = 8,
                     h4("Biá»u Äá» GDP vÃ  má»©c tÄng trÆ°á»ng háº±ng nÄm", align = "center", style = "color:#91186f"),
                     plotlyOutput("combineChart"))
      ),
      
      tabItem(tabName = "linechart",
              
              column(width = 6,
                     h3("Kim ngáº¡ch xuáº¥t kháº©u vÃ  nháº­p kháº©u 1986 - 2020", align = "center", style = "color:#ff5e03"),
                     dygraphOutput("xuatnhapkhau")),
              
              column(width = 6,
                     h3("GiÃ¡ trá» xuáº¥t siÃªu 1985 - 2020", align = "center", style = "color:#ff5e03"),
                     plotlyOutput("xuatsieu")),
              
              column(width = 6,
                     h3("Khoáº£n chi ngÃ¢n sÃ¡ch cho cÃ¡c lÄ©nh vá»±c nÄm 2017 - USD", align = "center", style = "color:#ff5e03"),
                     plotlyOutput("khoanchiGDP")),
              
              column(width = 6,
                     h3("Biá»u Äá» lÆ°á»£ng CO2 vÃ  tá»c Äá» gia tÄng tá»« nÄm 1960 - 2020", align = "center", style = "color:#ff5e03"),
                     plotlyOutput("combineCO2"))
              
      ),
      
      tabItem(tabName = "modelPopulation",
              column(width = 6,
                     column(width = 12,
                            h3("Biá»u Äá» dá»± ÄoÃ¡n ÄÃ¢n sá» vÃ  tá»c Äá» tÄng trÆ°á»ng tá»« nÄm 2020 - 2040", align = "center", style = "color:#128632"),
                            plotlyOutput("modelPop")),
                     
                     column(width = 12,
                            h3("Dá»± ÄoÃ¡n sá»± gia tÄng dÃ¢n sá» vá»i Time Series Model", align = "center", style = "color:#128632"),
                            plotOutput("timeseriesDS"))
              ),
              
              column(width = 6,
                     h4("Dá»¯ liá»u chi tiáº¿t Time series model", align = "center", style = "color:#264186"),
                     dataTableOutput("tableDanSo")
              )
      ),
      
      tabItem(tabName = "modelGDP",
              column(width = 6,
                     column(width = 12,
                            h3("Biá»u Äá» dá»± ÄoÃ¡n GDP vÃ  tá»c Äá» tÄng truá»ng tá»« nÄm 2020 - 2040", align = "center", style = "color:#128632"),
                            plotlyOutput("modelGDP")),
                     
                     column(width = 12,
                            h3("Dá»± ÄoÃ¡n sá»± gia tÄng GDP vá»i Time Series Model", align = "center", style = "color:#128632"),
                            plotOutput("timeseriesGDP"))
              ),
              
              column(width = 6,
                     h4("Dá»¯ liá»u chi tiáº¿t Time series model ", align = "center", style = "color:#264186"),
                     dataTableOutput("tableGDP")
              )
      )
    )
  ))
)