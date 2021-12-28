library(ggplot2)
library(GGally)
library(ggthemes)
library(pdftools)
library(tm)
library(stringi)
library(stringr)
library(data.table)

Sys.setlocale("LC_ALL",locale = "Turkish")

files <- list.files(pattern = "pdf$")
opinions <- lapply(files, pdf_text)

corp <- Corpus(URISource(files),
              readerControl = list(reader = readPDF))

opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(26, Inf))))

years <- c(1996:2021)

#Enflasyon hedeflemesi*

enfhed <- stri_count_fixed(opinions, "enflasyon hedeflemesi")
enfhed[22] <- 1
  
DTW <- data.table(time =years, data = enfhed)

enflabs <- labs(title = "Frequency of the Word 'Enflasyon Hedeflemesi'",
                 x = "Years", y = "Frequency",
                 caption = "Annual Economic Programmes, Turkey")



ggplot(DTW, aes(x=time, y=data,group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + enflabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)


#sabit kur rejimi(berbat)

sabkur <- stri_count_fixed(opinions, "sabit döviz kuru")

DTD <- data.table(time =years, data = sabkur)

ggplot(DTD, aes(x=time, y=data, group = 1))+geom_line(size=1)


#enflasyon*

enflas <- stri_count_fixed(opinions, "enflasyon")
enflas[22] <- 45
  
DTE <- data.table(time =years, data = enflas)

ratelabs <- labs(title = "Frequency of the Word 'Enflasyon'",
                 x = "Years", y = "Frequency",
                 caption = "Annual Economic Programmes, Turkey")



ggplot(DTE, aes(x=time, y=data,group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + ratelabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)


#döviz kuru*


dovkur <- stri_count_fixed(opinions, "döviz kuru")
dovkur[22]<-7

DTK <- data.table(time =years, data = dovkur)

exchangelabs <- labs(title = "Frequency of the Word 'D?viz Kuru'",
                 x = "Years", y = "Frequency",
                 caption = "Annual Economic Programmes, Turkey")


ggplot(DTK, aes(x=time, y=data,group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + exchangelabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)






#issizlik(berbat)

unemp <- stri_count_fixed(opinions, "issizlik")
unemp[22] <- 28
DTU <- data.table(time =years, data = unemp)

unemplabs <- labs(title = "Frequency of the Word 'Issizlik'",
                 x = "Years", y = "Frequency",
                 caption = "Annual Economic Programmes, Turkey")

ggplot(DTU, aes(x=time, y=data,group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + unemplabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)

#Dis bor?*


disborc <- stri_count_fixed(opinions, "dis bor?")

DTDB <- data.table(time =years, data = disborc)

dblabs <- labs(title = "Frequency of the Word 'Dis Bor?'",
                  x = "Years", y = "Frequency",
                  caption = "Annual Economic Programmes, Turkey")

ggplot(DTDB, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + dblabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)


#IMF*

IMF <- stri_count_fixed(opinions, "IMF")
IMF[22]<-3

DTimf <- data.table(time =years, data = IMF)

imflabs <- labs(title = "Frequency of the Word 'IMF'",
               x = "Years", y = "Frequency",
               caption = "Annual Economic Programmes, Turkey")

ggplot(DTimf, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + imflabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)

#bddk*

bddk <- stri_count_fixed(opinions, "BDDK")
bddk[22]<- 17
DTbddk <- data.table(time =years, data = bddk)

bddklabs <- labs(title = "Frequency of the Word 'BDDK'",
                x = "Years", y = "Frequency",
                caption = "Annual Economic Programmes, Turkey")

ggplot(DTbddk, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + bddklabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)


#merkez bankasi*


cb <- stri_count_fixed(opinions, "Merkez Bankasi")
cb[4]<-55
cb[22]<- 20

DTcb <- data.table(time =years, data = cb)

cblabs <- labs(title = "Frequency of the Word 'Merkez Bankasi'",
                 x = "Years", y = "Frequency",
                 caption = "Annual Economic Programmes, Turkey")

ggplot(DTcb, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + cblabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)

#turizm*

turism <- stri_count_fixed(opinions, "turizm")
turism[22]<-65

DTtur <- data.table(time =years, data = turism)

turlabs <- labs(title = "Frequency of the Word 'Turizm'",
               x = "Years", y = "Frequency",
               caption = "Annual Economic Programmes, Turkey")

ggplot(DTtur, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + turlabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)


#cari a?ik(k?t?)

CAD <- stri_count_fixed(opinions, "cari a?ik")

DTcad <- data.table(time =years, data = CAD)

cadlabs <- labs(title = "Frequency of the Word 'Cari A?ik'",
                x = "Years", y = "Frequency",
                caption = "Annual Economic Programmes, Turkey")

ggplot(DTcad, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + cadlabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)



#yabanci sermaye*


yabser <- stri_count_fixed(opinions, "yabanci sermaye")
yabser[4]<-25
yabser[22]<-3

DTyabser <- data.table(time =years, data = yabser)

yabserlabs <- labs(title = "Frequency of the Word 'Yabanci Sermaye'",
                x = "Years", y = "Frequency",
                caption = "Annual Economic Programmes, Turkey")

ggplot(DTyabser, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + yabserlabs + 
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)



#faiz orani*


faiz <- stri_count_fixed(opinions, "faiz oran")


DTfaiz<- data.table(time =years, data = faiz)

faizlabs <- labs(title = "Frequency of the Word 'Faiz Orani'",
                   x = "Years", y = "Frequency",
                   caption = "Annual Economic Programmes, Turkey")

ggplot(DTfaiz, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + faizlabs + 
  scale_x_continuous(breaks=seq(1998.00,2020.00,by=1))+
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)

#GSYH(berbat)

gdp <- stri_count_fixed(opinions, "GSYH")


DTGDP<- data.table(time =years, data = gdp)

gdplabs <- labs(title = "Frequency of the Word 'GSYH'",
                 x = "Years", y = "Frequency",
                 caption = "Annual Economic Programmes, Turkey")

ggplot(DTGDP, aes(x=time, y=data, group = 1, color="Exchange Rate")) + 
  geom_line(size=1.5) + 
  scale_color_manual(values="Blue4") +
  theme_minimal() + gdplabs + 
  scale_x_continuous(breaks=seq(1998.00,2020.00,by=1))+
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0, size = rel(1), color = "gray20"))+
  geom_vline(xintercept= c(1999,2001,2009,2019), alpha = 0.4,
             color = "darkgreen", size = 1.8)




