
library(devtools)
library(data.table)
library(CBRT)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(haven)
library(GGally)
library(mice)
library(stargazer)
library(WDI)


### CAD

myCBRTKey <- "LI8X1maguQ"

searchCBRT("deficit")


data <- getDataSeries(c("TP.ODANA6.Q01"), freq = 8)
old <- names(data)[1:2]
new <- c("Year","CAD")
setnames(data, old, new)

ylist <- seq(1996, 2020, by = 1)
data_cad <- subset(data, Year %in% ylist)


cad_labs <- labs(title = "Current Account Deficit",
                     subtitle = "Turkey (Yearly)",
                     x = "Years", y = "Current Account Deficit (Million $)",
                     caption = "Source: CBRT Electronic Data Delivery System")

pbar <- ggplot(data = data_cad, mapping = aes(x = Year, weight = CAD ))+
  cad_labs+
  geom_bar()+
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20")) + 
  guides(fill=guide_legend(title="Years"))+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))

  

  
plot(pbar)

### unemployment


WDIsearch(string = "unemployment", field = "name", short = TRUE, cache = NULL)


data_unemp <- WDI::WDI(country=c("TUR"), indicator=c("SL.UEM.TOTL.NE.ZS"), start="1996",end="2019")
setnames(data_unemp,"SL.UEM.TOTL.NE.ZS","Unemployment")

unemp_labs <- labs(title = "Unemployment Rate",
                   subtitle = "Turkey (Yearly)",
                   x = "Years", y = "Unemployment, total( % of total labor force)",
                   caption = "Source: World Development Indicators")

unemp_line <- ggplot(data = data_unemp, mapping = aes(x = year, y = Unemployment, color="Unemployment" ))+
  geom_line(size = 1.5)+
  theme_minimal()+
  unemp_labs+
  scale_color_manual(values="Red4")+
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00,2019.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))

plot(unemp_line)



### External Debt


data_disborc <- getDataSeries(c("TP.DB.B01"), freq = 8)
old <- names(data_disborc)[1:2]
new <- c("Year","External_Debt")
setnames(data_disborc, old, new)


ylist2 <- seq(1996, 2020, by = 1)
data_disborc2 <- subset(data_disborc, Year %in% ylist2)

data_disborc2[, Debt := External_Debt/1000]

disborc_labs <- labs(title = "Gross External Debt",
                   subtitle = "Turkey (Yearly)",
                   x = "Years", y = "Total External Debt (Billion $)",
                   caption = "Source: CBRT Electronic Data Delivery System")

line_3 <- ggplot(data = data_disborc2, mapping = aes(x = Year, y = Debt, color="Debt" ))+
  geom_line(size = 1.5)+
  theme_minimal()+
  disborc_labs+
  scale_color_manual(values="Red4")+
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00,2019.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))

  

plot(line_3)

### tourism


WDIsearch(string = "tourism", field = "name", short = TRUE, cache = NULL)

data_tourism <- WDI::WDI(country=c("TUR"), indicator=c("ST.INT.RCPT.CD"), start="1996",end="2020")
setnames(data_tourism,"ST.INT.RCPT.CD","turizm")

dt_turizm <- data.table(data_tourism)
dt_turizm[, gelir_bin := turizm/1000000]

turizm_labs <- labs(title = "International Tourism Receipts",
                   subtitle = "Turkey (Yearly)",
                   x = "Years", y = "Total Receipts (Million $)",
                   caption = "Source: World Bank")

turizm_line <- ggplot(data = dt_turizm, mapping = aes(x = year, y = gelir_bin, color="gelir_bin" ))+
  geom_line(size = 1.5)+
  theme_minimal()+
  turizm_labs+
  scale_color_manual(values="Red4")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  theme(legend.position = "none")+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00,2016.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))


plot(turizm_line)

### Exchange Rate

rate <- getDataSeries("TP.DK.USD.S.YTL",freq = 6,startDate = "01-01-1996",endDate = "01-01-2020")
denge <- getDataSeries("TP.OD.Z01",freq = 6,startDate = "01-01-2002",endDate = "01-01-2015")

ratelabs <- labs(title = "Exhange Rate",
                 subtitle = "Turkey (Quarterly)",
                 x = "Years", y = "US Dollar and Turkish Lira Exchange Rate",
                 caption = "Source: CBRT Electronic Data Delivery System")

exhange_rate <- ggplot(rate, aes(x=time, y=TP.DK.USD.S.YTL, color="Exchange Rate")) +
  geom_line(size=1.5) + 
  scale_color_manual(values="Red4") + 
  theme_minimal() + 
  ratelabs +  
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00,2019.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))



plot(exhange_rate)


### Inflation rate

scatterhw <- WDI::WDI(country=c("TUR"), indicator=c("FR.INR.RINR", "NY.GDP.DEFL.KD.ZG"), start="1996",end="2020")
setnames(scatterhw,"NY.GDP.DEFL.KD.ZG","INF")

inflabs <- labs(title = "Inflation Rates of Turkey (GDP Deflector)",
                subtitle = "Turkey (Yearly)",
                x = "Years", y = "Inflation Rate (%)",
                caption = "Source: World Development Indicators")
inflation_rate <- ggplot(scatterhw, aes(x=year, y=INF, color="inf")) + geom_line(size=1.5) + inflabs +
  theme_minimal() + 
  scale_color_manual(values="Red4") + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00,2019.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))


plot(inflation_rate)

### Real Effective Exchange Rate

rate2 <- getDataSeries("TP.DK.REER4",freq = 6,startDate = "01-01-1995",endDate = "01-01-2010")
rate2

rate2labs <- labs(title = "Real Effective Exchange Rate",
                  subtitle = "Turkey (Quarterly)",
                  x = "Years", y = "Effective Exchange Rate (%)",
                  caption = "Source: CBRT Electronic Data Delivery System")
exchange_rate <- ggplot(rate2, aes(x=time, y=TP.DK.REER4, color="Rate")) + geom_line(size=1.5) + rate2labs +
  theme_minimal() + 
  scale_color_manual(values="Red4") + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))



plot(exchange_rate)


### GDP 1996-2020


GDPdata <- WDI::WDI(indicator= c("NY.GDP.MKTP.PP.CD", "NY.GDP.MKTP.KD.ZG", "NY.GDP.MKTP.KD" , "FR.INR.DPST") , country= c("TUR"), start="1996", end="2019")
setnames(GDPdata,"NY.GDP.MKTP.KD", "gdpcons")
GDPdata2 <- data.table(GDPdata)
GDPdata2[, xxx := gdpcons/1000000000]




labsb <- labs(x="Years", y="Gross Domestic Product (Billion 2010 US$)",
              title = "Gross Domestic Product",
              subtitle= "Turkey (Yearly)" ,
              caption = "Source: World Development Indicators") 


Graph2 <- ggplot(GDPdata2, aes(x=year, y= xxx, color="gdpcons"))+
  geom_line(size = 1.5)+
  theme_minimal()+
  scale_color_manual(values = "Red4")+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00,2019.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  theme(legend.position = "none")+
  labsb+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))+
  scale_y_continuous(breaks=seq(0,1300,by=75))



plot(Graph2)


### GDP Growth

setnames(GDPdata,"NY.GDP.MKTP.KD.ZG", "gdpgrowth")


labsc <- labs(x="Years", y="GDP Growth (%)",
              title = "GDP Growth",
              subtitle= "Turkey (Yearly)",
              caption = "Source: World Development Indicators") 

Graph3 <- ggplot(GDPdata, aes(x=year, y= gdpgrowth, color="gdpgrowth"))+
  geom_line(size = 1.5)+
  theme_minimal()+
  scale_color_manual(values = "Red4")+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00,2019.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  labsc+
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))



plot(Graph3)

### 


setnames(GDPdata,"FR.INR.DPST", "interestrate")


labsd <- labs(x="Years", y="Deposit Interest Rate (%)",
              title = "Deposit Interest Rate",
              subtitle= "Turkey (Yearly)",
              caption = "Source: World Development Indicators") 

Graph4 <- ggplot(GDPdata, aes(x=year, y= interestrate, color="interestrate"))+
  geom_line(size = 1.5)+
  theme_minimal()+
  scale_color_manual(values="Red4")+
  geom_vline(xintercept= c(1998.00,2001.00,2009.00,2019.00), alpha = 0.4,
             color = "darkgreen", size = 1.8)+
  labsd+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(1996.00,2020.00,by=1))



plot(Graph4)

