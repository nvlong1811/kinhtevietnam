#4: ------------ Cau hoi nghien cuu ------------
install.packages("viridis")
#library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(latticeExtra)
library(dygraphs)
library(xts)
library(lubridate)
library(tidyverse)
library(babynames)
library(viridis)
library(forecast)
library(sweep)

shinyServer(function(input, output) {
  
  output$mytable1 = renderDataTable({
    
    data_fully[,, drop = FALSE]
  }, options = list(scrollX = TRUE))
  
  # sorted columns are colored now because CSS are attached to them
  output$myplot = renderPlotly({
    data_date$Year <- as.Date(data_date$X)
    code<- input$code
    
    ggplotly(ggplot(data,aes_string(x="Year",y=code )) +   
               geom_area(fill="#e5e97e", alpha=0.5) +
               geom_line(color="#ffb20f") +
               ylab(data_fully[2,code]) +
               labs(title=data_fully[1,code]) +
               theme_ipsum()
    )})
  
  observe({
    #lay cac gia tri theo slide year
    year<-input$sliderYear
    yearfter <- subset(data, Year == year)    
    cn <- yearfter[1, "NV.IND.TOTL.ZS"]
    nn <- yearfter[1, "NV.AGR.TOTL.ZS"]
    dv <- 100-(cn+nn)
    totalGDP <- yearfter[1, "NY.GDP.MKTP.CD"]
    peopleGDP <- yearfter[1, "NY.GDP.PCAP.CD"]
    tichluyUSD <- yearfter[1, "NE.GDI.TOTL.ZS"]  
    tmhanghoa <- yearfter[1, "TG.VAL.TOTL.GD.ZS"]  
    dautuNN <- yearfter[1, "BX.KLT.DINV.CD.WD"]
    lamp <- yearfter[1, "NY.GDP.DEFL.KD.ZG"]
    lampgia <- yearfter[1, "FP.CPI.TOTL.ZG"]
    
    year2017 <- subset(data, Year == 2017)
    giaoduc <- year2017[1, "SE.XPD.TOTL.GD.ZS"]
    quocphong <- year2017[1, "MS.MIL.XPND.GD.ZS"]
    nghiencuu <- year2017[1, "GB.XPD.RSDV.GD.ZS"]
    
    
    
    #bieu do tron GDP
    output$PieGDP = renderPlot({
      datapie <- data.frame(
        group=c("Công nghiệp - Xây dựng","Nông - Lâm - Ngư nghiệp","Dịch vụ"),
        value=c(cn,nn,dv)
      )
      ggplot(datapie, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        labs(title=year) +
        theme_void()})
    
    output$congnghiep = renderText({sum(cn)})
    output$nongnghiep = renderText({sum(nn)})
    output$dichvu = renderText({sum(dv)})     
    output$totalGDP = renderText({sum(totalGDP/1000000)})
    output$peopleGDP = renderText({sum(peopleGDP)})
    output$tichluyUSD = renderText({sum(tichluyUSD)})
    output$tmhanghoa = renderText({sum(tmhanghoa)})
    output$dautuNN = renderText({sum(dautuNN/1000000)})
    output$lamp = renderText({sum(lamp)})
    output$lampgia = renderText({sum(lampgia)})
    
    output$khoanchiGDP = renderPlotly({
      data <- data.frame(
        name = c("Giáo dục", "Quốc phòng", "Nghiên cứu"),
        value= c(((giaoduc*totalGDP)/100)/1000000, ((quocphong*totalGDP)/100)/1000000, ((nghiencuu*totalGDP)/100)/1000000)
      )
      ggplotly(ggplot(data, aes(x=name, y=value)) + 
                 geom_bar(stat = "identity", fill="#156953") +
                 coord_flip() +
                 xlab("") +
                 ylab("Triệu USD") 
      )})
  })
  
  output$combineChart = renderPlotly({
    ggplotly(ggplot(data)  + 
               geom_bar(aes(x=data$Year, y=data$NY.GDP.MKTP.CD/1000000000),stat="identity", fill="#df622d")+
               geom_line(aes(label = data$NY.GDP.MKTP.KD.ZG,x=data$Year, y=20*data$NY.GDP.MKTP.KD.ZG),stat="identity",color="#1bb11b",size=1)+
               scale_y_continuous(sec.axis = sec_axis(~./10, name = "% Hang nam"))+
               
               labs(title= NULL,
                    x="Year",y="Tỉ USD")
    )})
  
  #Bieu do su tuong quan cua GDP va toc do tang truong
  output$combineChart = renderPlotly({
    dataGDP <- subset(data, Year >= 1980)
    ggplotly(ggplot(dataGDP)  + 
               geom_bar(aes(x=dataGDP$Year, y=dataGDP$NY.GDP.MKTP.CD/1000000000),stat="identity", fill="#7d3586")+
               geom_line(aes(label = dataGDP$NY.GDP.MKTP.KD.ZG, x=dataGDP$Year, y=20*dataGDP$NY.GDP.MKTP.KD.ZG),stat="identity",color="#0a6dd4",size=1)+
               scale_y_continuous(sec.axis = sec_axis(~./10, name = "% Hang nam"))+
               
               labs(title= NULL,
                    x="Year",y="Tỉ USD")
    )})
  
  #Bieu do kim ngach xuat, nhap khau
  output$xuatnhapkhau = renderDygraph({
    #lay cac gia tri GDP 
    data_date <- subset(data_date, Year >= 1980)
    year<-input$sliderYear
    data1 <- data.frame(
      time= as.Date(data_date$X),
      XuatKhau=data_date$NE.IMP.GNFS.ZS, 
      NhapKhau=data_date$NE.EXP.GNFS.ZS )
    
    don=xts( x=data1[,-1], order.by=data1$time)
    # Chart
    p <- dygraph(don) %>%
      dyRangeSelector(height = 20)})
  
  
  #tinh gia tri xuat sieu
  output$xuatsieu = renderPlotly({
    #lay cac gia tri GDP 
    data <- data.frame(
      year = data$Year,
      val= data$NE.EXP.GNFS.ZS -data$NE.IMP.GNFS.ZS
    )
    ggplotly(ggplot(data, aes(x=x) ) +
               geom_histogram( aes(x = year, y = val), fill="green" ,stat="identity") +
               #geom_label( aes(x=1990, y=8, label="Xuat sieu"), color="green") +
               geom_histogram( aes(x = year, y = val), fill= "#365463", color="#519ddd", stat="identity") +
               #geom_label( aes(x=1990, y=-8, label="Nhap sieu"), color="red") +
               theme_ipsum() +
               xlab("Year") +
               ylab("% GDP")
    )
  })
  
  #Mo hinh du doan dan so Viet Nam tu nam 2020 - 2040
  output$modelPop = renderPlotly({
    
    data_danso <- data.frame(
      year <- data$Year,
      danso <- data$SP.POP.TOTL,
      tangtruong <- data$SP.POP.GROW
    )
    
    year1 <- 2020
    for(i in 61:80) {
      year1<-year1+1
      
      danso1<-data_danso$danso[i]*(1+((data_danso$tangtruong[i])/100))
      congtangtruong<-0
      
      for(j in (i-4):i){
        congtangtruong <-congtangtruong+data_danso$tangtruong[j]
      }
      
      tangtruong1<-congtangtruong/5
      
      data_danso[nrow(data_danso) + 1,] = list(year=year1,danso=danso1, tangtruong=tangtruong1)
      
    }
    
    #tinh hieu cua dan so nam sau so voi nam truoc
    hieuso <- c(data_danso$danso[2]-  data_danso$danso[1])
    for (i in 2:nrow(data_danso) ){
      if(i==nrow(data_danso)){
        hieu<-NaN
      }
      else{
        hieu <- data_danso$danso[i+1]-  data_danso$danso[i]
      }
      hieuso <-append(hieuso, hieu )
      
    }
    
    data_hieuso <- data.frame(
      year <- data_danso$year,
      hieu_so <- hieuso,
      tangtruong <- data_danso$tangtruong
    )
    
    # Plot
    my_colorbar= ifelse(data_danso$year < 2021 ,"#43b8a0", "#88e2d0" )
    
    ggplotly(ggplot(data_danso)  + 
               geom_bar(aes(label=data_hieuso$hieu_so, x=data_danso$year, y=data_danso$danso/10^6),stat="identity", fill=my_colorbar)+
               
               geom_line(aes(label=data_hieuso$tangtruong, x=data_hieuso$year, y=((data_hieuso$tangtruong)*20)+30),stat="identity",color="#ff7a30",size=1)+
               
               scale_y_continuous(sec.axis = sec_axis(~./10, name = "% Hang nam"))+
               
               labs(title= NULL,
                    x="Year",y="(Trieu nguoi)") )})
  
  
  #Bieu do tinh toan so sanh gia tri CO2 va toc do gia tang
  output$combineCO2 = renderPlotly({
    
    co2 <- c(100 - ((data$EN.ATM.CO2E.PC[1]*100)/data$EN.ATM.CO2E.PC[2]))
    for (i in 2:nrow(data) ){
      if(i==nrow(data)){
        hieu<-NA
      }
      else{
        hieu <- 100 - ((data$EN.ATM.CO2E.PC[i]*100)/data$EN.ATM.CO2E.PC[i+1])  
      }
      co2 <-append(co2, hieu )
      
    }
    
    data_co2 <- data.frame(
      Year <- data$Year,
      GR.CO2 <- co2,
      EN.ATM.CO2E.PC <- data$EN.ATM.CO2E.PC
    )
    
    ggplotly(ggplot(data_co2)  + 
               geom_bar(aes( x=data_co2$Year, y=data_co2$EN.ATM.CO2E.PC),stat="identity", fill="#475e69")+
               geom_line(aes(label = data_co2$GR.CO2, x=data_co2$Year, y=(data_co2$GR.CO2)/50),stat="identity",color="#a1223b",size=1)+
               labs(title= NULL,
                    x="Year",y="Met tan/nguoi")
    )})
  
  #Mo hinh du doan gia tri GDP 2020-2040
  output$modelGDP = renderPlotly({
    dataGDP <- subset(data, Year >= 1980)
    
    data_GDP <- data.frame(
      year <- dataGDP$Year,
      GDP <- dataGDP$NY.GDP.MKTP.CD,
      tangtruong <- dataGDP$NY.GDP.MKTP.KD.ZG
    )
    
    year1 <- 2020
    for(i in nrow(data_GDP):(nrow(data_GDP)+19)) {
      year1<-year1+1
      
      GDP1<-data_GDP$GDP[i]*(1+((data_GDP$tangtruong[i])/100))
      congtangtruong<-0
      
      for(j in (i-4):i){
        congtangtruong <-congtangtruong+data_GDP$tangtruong[j]
      }
      
      tangtruong1<-congtangtruong/5
      
      data_GDP[nrow(data_GDP) + 1,] = list(year=year1,GDP=GDP1, tangtruong=tangtruong1)
      
    }
    #tinh hieu cua dan so nam sau so voi nam truoc
    hieuso <- c(data_GDP$GDP[2]-  data_GDP$GDP[1])
    for (i in 2:nrow(data_GDP) ){
      if(i==nrow(data_GDP)){
        hieu<-NaN
      }
      else{
        hieu <- data_GDP$GDP[i+1]-  data_GDP$GDP[i]
      }
      hieuso <-append(hieuso, hieu )
      
    }
    
    data_hieuso <- data.frame(
      year <- data_GDP$year,
      hieu_so <- hieuso,
      tangtruong <- data_GDP$tangtruong
    )
    
    colorfill= ifelse(data_GDP$year < 2021 ,"#72156e", "#72a56e" )
    
    ggplotly(ggplot(data_GDP)  + 
               geom_bar(aes(x=data_GDP$year, y=(data_GDP$GDP)/10^9),stat="identity", fill="#6499ca")+
               geom_line(aes(label = data_hieuso$tangtruong, x=data_hieuso$year, y=(data_hieuso$tangtruong)*20),stat="identity",color="#72156e",size=1)+ 
               labs(title= NULL,
                    x="Year",y="(Tỉ USD)"))})
  
  
  
  #Mo hinh ap dung time series cho DAN SO
  observe({
    #du lieu dan so
    danso <- data$SP.POP.TOTL/10^6
    
    # tao ra doi tuong time series
    # khai bao diem bat dau la nam 1960, khoang cach la 1 nam
    mts <- ts(danso, start = decimal_date(ymd("1960-01-01")),
              frequency = 1)
    fit <- auto.arima(mts)
    
    m <- data.frame()
    
    # du doan 30 nam tiep theo
    dudoan <- forecast(fit, 20)
    Fcast_df <- dudoan %>% 
      sweep::sw_sweep(.) %>% 
      filter(key == "forecast") %>% 
      select(-key)
    
    output$timeseriesDS = renderPlot({   
      plot(dudoan, xlab ="Year",
           ylab ="Triệu người",
           main="1960 - 2040",col.main ="darkgreen")})
    
    output$tableDanSo = renderDataTable({
      Fcast_df[,, drop = FALSE]
    }, options = list(scrollX = TRUE))
  })
  
  
  
  #Mo hinh ap dung time series cho GDP
  observe({
    dataGDP <- subset(data, Year >= 1980)
    #du lieu dan so
    GDP <- dataGDP$NY.GDP.MKTP.CD/10^9
    
    # tao ra doi tuong time series
    # khai bao diem bat dau la nam 1980, khoang cach la 1 nam
    mts <- ts(GDP, start = decimal_date(ymd("1980-01-01")),
              frequency = 1)
    fit <- auto.arima(mts)
    
    # du doan 30 nam tiep theo
    dudoan <- forecast(fit, 20)
    Fcast_df <- dudoan %>% 
      sweep::sw_sweep(.) %>% 
      filter(key == "forecast") %>% 
      select(-key)
    
    output$timeseriesGDP = renderPlot({
      plot(dudoan, xlab ="Year",
           ylab ="Tỉ USD ",
           main ="1960 - 2040", col.main ="darkgreen")})
    
    output$tableGDP = renderDataTable({
      Fcast_df[,, drop = FALSE]
    }, options = list(scrollX = TRUE))
  })
}
)