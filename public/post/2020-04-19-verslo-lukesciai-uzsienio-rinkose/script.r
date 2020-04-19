if(!require("lubridate")) install.packages("lubridate"); library("lubridate") # for date conversion
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse") # tidyr,dplyr,ggplot
if(!require("zoo")) install.packages("zoo"); library("zoo") # for date conversion
if(!require("reshape2")) install.packages("reshape2"); library("tidyverse") ## for data conversion
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra") ## for grid.arrange
if(!require("data.table")) install.packages("data.table"); library("data.table") ## for data conversion
if(!require("eurostat")) install.packages("eurostat"); library("eurostat") ## for eurostat data import
if(!require("ggthemes")) install.packages("ggthemes"); library("ggthemes") ## ggplot themes

## Data import
# GDP

gdp <- get_eurostat("namq_10_gdp", filters=list(s_adj="SCA",
                                                geo=c("EA19","DE", "SE", "UK"),
                                                na_item="B1GQ",
                                                unit="CLV15_MEUR"))
gdp <- gdp%>% select(time,values, geo)%>%
        group_by(geo)%>% 
        spread(geo,values)%>%
        filter(time>="2002-01-01")%>%
        mutate(DE=(DE-lag(DE,4))/lag(DE,4)*100,
               EA19=(EA19-lag(EA19,4))/lag(EA19,4)*100,
               SE=(SE-lag(SE,4))/lag(SE,4)*100,
               UK=(UK-lag(UK,4))/lag(UK,4)*100)
gdp$time <- as.yearqtr(gdp$time)
gdp<- filter(gdp, time>="2008-01-01")


# LT Eksportas pagal valstybes
Sys.setlocale("LC_ALL","Lithuanian")
df <- read.delim ("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/lteksport.csv", header=TRUE, stringsAsFactors = FALSE, sep=";", encoding="UTF-8")
df <- df%>% rename(grupe=X.U.FEFF.aa)%>%
        mutate(JAV11=(parse_number(df$JAV.1)/100),
               JK11=(parse_number(df$JK.1)/100),
               SE11=(parse_number(df$SE)/100),
               DE11=parse_number(df$DE)/100)%>%
        select(grupe, JAV11, JK11,SE11,DE11)


# Pagrindinis Lietuvos eksportas

dff <- read.csv("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/Eksportas.csv", header=TRUE, encoding="UTF-8", stringsAsFactors = FALSE)
Sys.setlocale("LC_ALL","Lithuanian")
dff <- dff%>% 
        filter(Šalys.ir.teritorijos.Kodas!="QS")%>%
        top_n(30)%>%
        select(Šalys.ir.teritorijos.Pavadinimas, X)%>%
        mutate(Proc=as.numeric(sub("%","",X)))%>%
        mutate_if(is.numeric, ~round(., 1))

# Ekonomikos atvirumas

exp <- get_eurostat("tet00003", filters=list(time="2019"))
exp <- exp%>% filter(geo%in%c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES","BG", "HR", "CZ", "DK", "HU", "RO", "SE","EL","EU28"))%>%
        select(geo,values)%>%
        rename(export=values)
imp <- get_eurostat("tet00004", filters=list(time="2019"))
imp <- imp%>% filter(geo%in%c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES","BG", "HR", "CZ", "DK", "HU","RO", "SE","EL","EU28"))%>%
        select(geo,values)%>%
        rename(import=values)
df <- merge(exp,imp)
df <- df%>%mutate(trade=import+export)%>%
        mutate_if(is.numeric, ~round(., 0))

# Vokietijos PMI ir BVP

Sys.setlocale("LC_ALL", "english")
german<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/GermanyManufacturing.txt", header=FALSE,stringsAsFactors = FALSE)
german<- german%>%select(V1,V2)%>%
        rename(PMI=V2, Date=V1)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
german$Date <- format(as.Date(german$Date, format="%b %d,%Y"),"%Y.%m.%d")
german$Date <- as.Date(german$Date, format="%Y.%m.%d")
german$Date <- german$Date %m+% months(-1)


germser<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/GermanyServices.txt", header=FALSE, stringsAsFactors = FALSE)
germser<- germser%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
germser$Date <- format(as.Date(germser$Date, format="%b %d,%Y"),"%Y-%m-%d")
germser$Date <- as.Date(germser$Date, format="%Y-%m-%d")
germser$Date <- germser$Date %m+% months(-1)


germcom<-read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/germanycomposite.txt", header=TRUE, stringsAsFactors = FALSE)
germcom<- germcom %>%select(Date,X)%>%
        rename(PMI=X)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
germcom$Date <- format(as.Date(germcom$Date, format="%b %d,%Y"),"%Y.%m.%d")
germcom$Date <- as.Date(germcom$Date, format="%Y.%m.%d")
germcom$Date <- germcom$Date %m+% months(-1)

## Svedija PMI ir BVP

Sys.setlocale("LC_ALL", "english")
seman<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/SwedenManufacturing.txt", header=FALSE,stringsAsFactors = FALSE)
seman<- seman%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
seman$Date <- format(as.Date(seman$Date, format="%b %d,%Y"),"%Y.%m.%d")
seman$Date <- as.Date(seman$Date, format="%Y.%m.%d")
seman$Date <- seman$Date %m+% months(-1)

seser<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/SwedenServices.txt", header=FALSE, stringsAsFactors = FALSE)
seser<- seser%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
seser<-na.omit(seser)
seser$Date <- format(as.Date(seser$Date, format="%b %d,%Y"),"%Y-%m-%d")
seser$Date <- as.Date(seser$Date, format="%Y-%m-%d")
seser$Date <- seser$Date %m+% months(-1)


## JAV PMI ir BVP


Sys.setlocale("LC_ALL", "english")
usman <- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/us_pmi_man.csv", header=TRUE, stringsAsFactors = FALSE, sep=",")
usman <- usman %>%filter(Date>="2008-01-01")%>% select(Date, PMI)
usman$Date <- as.Date(usman$Date, format="%Y-%m-%d")


usser<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/usservices.txt", header=FALSE, stringsAsFactors = FALSE)
usser<- usser%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
usser$Date <- format(as.Date(usser$Date, format="%b %d,%Y"),"%Y-%m-%d")
usser$Date <- as.Date(usser$Date, format="%Y-%m-%d")
usser$Date <- usser$Date %m+% months(-1)

uscom <- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/JAVcomposite.txt", header=TRUE, stringsAsFactors = FALSE)
uscom<- uscom %>%select(Date,X)%>%
        rename(PMI=X)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
uscom$Date <- format(as.Date(uscom$Date, format="%b %d,%Y"),"%Y.%m.%d")
uscom$Date <- as.Date(uscom$Date, format="%Y.%m.%d")
uscom$Date <- uscom$Date %m+% months(-1)

usgdp <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1075&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDPC1&scale=left&cosd=2007-04-30&coed=2019-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=pc1&vintage_date=2020-04-14&revision_date=2020-04-14&nd=2015-01-01", header=TRUE, stringsAsFactors = FALSE)
usgdp$DATE <- as.Date(usgdp$DATE, format="%Y-%m-%d")
usgdp$DATE <- as.yearqtr(usgdp$DATE)
usgdp<- filter(usgdp, DATE>="2008-01-01")

##JK eksportas ir PMI
Sys.setlocale("LC_ALL", "English")
ukman<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/UKManufacturing.csv", header=TRUE,stringsAsFactors = FALSE)
ukman[,1]<- rownames(ukman)
rownames(ukman) <- 1:nrow(ukman)
ukman<- ukman%>%select(Date,X)%>%
        rename(PMI=X)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
ukman$Date <- format(as.Date(ukman$Date, format="%b %d,%Y"),"%Y.%m.%d")
ukman$Date <- as.Date(ukman$Date, format="%Y.%m.%d")
ukman$Date <- ukman$Date %m+% months(-1)

ukser<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/UKServices.csv", header=FALSE, stringsAsFactors = FALSE)
ukser<- ukser%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
ukser$Date <- format(as.Date(ukser$Date, format="%b %d,%Y"),"%Y-%m-%d")
ukser$Date <- as.Date(ukser$Date, format="%Y-%m-%d")
ukser$Date <- ukser$Date %m+% months(-1)

ukcom<-read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/UKComposite.csv", header=TRUE, stringsAsFactors = FALSE)
ukcom<- ukcom %>%select(Date,X)%>%
        rename(PMI=X)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
ukcom$Date <- format(as.Date(ukcom$Date, format="%b %d,%Y"),"%Y-%m-%d")
ukcom$Date <- as.Date(ukcom$Date, format="%Y-%m-%d")
ukcom$Date <- ukcom$Date %m+% months(-1)


## Grafikai


## nelygybe

ineq <- get_eurostat("ilc_di11b", filters=list(sex="T",
                                               age="Y_LT65"))
ineq <- ineq%>% filter(time=="2018-01-01", 
                       geo%in%c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES","BG", "HR", "CZ", "DK", "HU", "RO", "SE","EL","EU28"))
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/income.png", width = 10, height = 6, units = 'in', res = 100)                       
Sys.setlocale("LC_ALL","Lithuanian")
ggplot(ineq, aes(x=reorder(geo, -values), y=values))+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=45,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Valstybė", y="Eksporto dalis %", title="Pajamų pasiskirstymas tarp 20% didžiausias ir mažiausias pajamas gaunančių gyventojų", 
             subtitle="Duomenų šaltinis: Eurostat, skaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(values,"%")), position=position_dodge(width=0.9), vjust=-0.25, size=2.5)
dev.off()

## EKSPORTAS

png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/Eksportas.png", width = 10, height = 6, units = 'in', res = 100)
Sys.setlocale("LC_ALL","Lithuanian")
ggplot(dff, aes(x=reorder(Šalys.ir.teritorijos.Pavadinimas, -Proc), y=Proc))+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=45,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Valstybė", y="Eksporto dalis %", title="Pagrindinės Lietuvos eksporto partnerės", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas, skaičiavimai: Corona-stat.lt")+
        ylim(0,10)+
        geom_text(aes(label=paste0(Proc,"%")), position=position_dodge(width=0.9), vjust=-0.25, size=2.5)
dev.off()


## Ekonomikos atvirumas

png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/atvirumas.png", width = 10, height = 6, units = 'in', res = 100)

Sys.setlocale("LC_ALL","Lithuanian")
ggplot(df, aes(x=reorder(geo, -trade),y=trade))+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=45,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Valstybė", y="Ekonomikos atvirumas [%]", title="ES valstybių ekonomikų atvirumas 2019m.", 
             subtitle="Duomenų šaltinis: Eurostat, Skaičiavimai: corona-stat.lt")+
        geom_text(aes(label=paste0(trade,"%")), vjust=-0.25, size=3)

dev.off()

## Eksportas į Vokietiją.


df1<- df%>%select(grupe,DE11)%>%top_n(20)   
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/DEeksportas.png", width = 10, height = 6, units = 'in', res = 100)
ggplot(df1, aes(x=reorder(grupe, DE11), y=DE11))+
        coord_flip()+
        geom_bar(stat="identity", fill="steelblue")+
        theme_bw()+
        theme(axis.text.x=element_text(angle=0,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Grupė", y="Eksporto dalis %", title="Lietuviškos kilmės eksportas į Vokietiją 2019m.", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas, skaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(DE11,"%")), position=position_dodge(width=0.9), hjust=-0.05)+
        scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5))
dev.off()


## Vokietija PMI, BVP


png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/Vokietija.png", width = 13, height =8 , units = 'in', res = 100)
Sys.setlocale("LC_ALL", "Lithuanian")
par(mfrow=c(2,1),oma = c(0, 0, 0, 2),xpd="NA")
plot(german$Date, german$PMI, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     main="PMI Vokietijoje 2008-2020m.", ylim=c(32.5,65))
abline(h=50, untf = FALSE, lty=3)
lines(germser$Date, germser$PMI, type="l", lwd=3, col="grey")
lines(germcom$Date, germcom$PMI,type="l", lwd=3, col="steelblue")
plot( gdp$time, gdp$DE, type="l", lwd=3, col="black",
      main="BVP Vokietijoje 2008-2020m.", sub="Duomenų šaltiniai: IHS Markit, Eurostat, skaičiavimai: Corona-stat.lt", xlab="Metai", ylab="BVP augimas, %", xlim=c(2008,2019.5))
abline(h=0, untf=FALSE, lty=3)
legend(2011,20 ,c("Gamybinis PMI","Paslaugų PMI", "Bendras PMI"),lty=c(1,1),bty = "n",lwd=3,col=c("black","grey","steelblue"), cex = 1, horiz = TRUE)
dev.off()

## Švedija eksportas



Sys.setlocale("LC_ALL","Lithuanian")
df1<- df%>%select(grupe,SE11)%>%top_n(20)
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/SEeksportas.png", width = 10, height = 6, units = 'in', res = 100)
ggplot(df1, aes(x=reorder(grupe, SE11), y=SE11))+
        coord_flip()+
        geom_bar(stat="identity", fill="steelblue")+
        theme_bw()+
        theme(axis.text.x=element_text(angle=0,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Grupė", y="Eksporto dalis %", title="Lietuviškos kilmės eksportas į Švediją 2019m.", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas, skaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(SE11,"%")), position=position_dodge(width=0.9), hjust=-0.05)+
        scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5))
dev.off()

## SVEDIJA


png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/Svedija.png", width = 13, height =8 , units = 'in', res = 100)
Sys.setlocale("LC_ALL", "Lithuanian")
par(mfrow=c(2,1),oma = c(0, 0, 0, 2),xpd="NA")
plot(seman$Date, seman$PMI, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     main="PMI Švedijoje 2008-2020m.")
abline(h=50, untf = FALSE, lty=3)
lines(seser$Date, seser$PMI, type="l", lwd=3, col="steelblue")
plot( gdp$time, gdp$SE, type="l", lwd=3, col="black",
      main="BVP Švedijoje 2008-2020m.", sub="Duomenų šaltiniai: IHS Markit, Eurostat, skaičiavimai: Corona-stat.lt", xlab="Metai", ylab="BVP augimas, %", xlim=c(2008,2019.5))
abline(h=0, untf=FALSE, lty=3)
legend(2011,23 ,c("Gamybinis PMI","Paslaugų PMI"),lty=c(1,1),bty = "n",lwd=3,col=c("black","steelblue"), cex = 1, horiz = TRUE)
dev.off()


## JAV EKsportas


Sys.setlocale("LC_ALL","Lithuanian")
df1<- df%>%select(grupe,JAV11)%>%top_n(20)
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/JAVeksportas.png", width = 10, height = 6, units = 'in', res = 100)
ggplot(df1, aes(x=reorder(grupe, JAV11), y=JAV11))+
        coord_flip()+
        geom_bar(stat="identity", fill="steelblue")+
        theme_bw()+
        theme(axis.text.x=element_text(angle=0,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Grupė", y="Eksporto dalis %", title="Lietuviškos kilmės eksportas į Jungtines Amerikos Valstijas 2019m.", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas, skaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(JAV11,"%")), position=position_dodge(width=0.9), hjust=-0.05)+
        scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5))
dev.off()




## JAV

png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/JAV.png", width = 13, height =8 , units = 'in', res = 100)
Sys.setlocale("LC_ALL", "Lithuanian")
par(mfrow=c(2,1),oma = c(0, 0, 0, 2),xpd="NA")
plot(usman$Date, usman$PMI, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     main="PMI JAV 2008-2020m.")
abline(h=50, untf = FALSE, lty=3)
lines(usser$Date, usser$PMI, type="l", lwd=3, col="steelblue")
lines(uscom$Date, uscom$PMI, type="l", lwd=3, col="grey")
plot( usgdp$DATE, usgdp$GDPC1_PC1, type="l", lwd=3, col="black",
      main="BVP JAV 2008-2020m.", sub="Duomenų šaltiniai: IHS Markit, ISM, Federal Reserve Bank, skaičiavimai: Corona-stat.lt", xlab="Metai", ylab="BVP augimas, %", xlim=c(2008,2019.8))
abline(h=0, untf=FALSE, lty=3)
legend(2010,15 ,c("Gamybinis PMI","Paslaugų PMI", "Bendras PMI"),lty=c(1,1),bty = "n",lwd=3,col=c("black","steelblue", "grey"), cex = 1, horiz = TRUE)
dev.off()



## JK EKSPORTAS


Sys.setlocale("LC_ALL","Lithuanian")
df1<- df%>%select(grupe,JK11)%>%top_n(20)
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/UKeksportas.png", width = 10, height = 6, units = 'in', res = 100)
ggplot(df1, aes(x=reorder(grupe, JK11), y=JK11))+
        coord_flip()+
        geom_bar(stat="identity", fill="steelblue")+
        theme_bw()+
        theme(axis.text.x=element_text(angle=0,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Grupė", y="Eksporto dalis %", title="Lietuviškos kilmės eksportas į Jungtinę Karalystę 2019m.", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas, skaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(JK11,"%")), position=position_dodge(width=0.9), hjust=-0.05)+
        scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5))
dev.off()

## JK



png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/UK.png", width = 13, height =8 , units = 'in', res = 100)
Sys.setlocale("LC_ALL", "Lithuanian")
par(mfrow=c(2,1),oma = c(0, 0, 0, 2),xpd="NA")
plot(ukman$Date, ukman$PMI, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     main="PMI JK 2008-2020m.")
abline(h=50, untf = FALSE, lty=3)
lines(ukser$Date, ukser$PMI, type="l", lwd=3, col="steelblue")
lines(ukcom$Date, ukcom$PMI, type="l", lwd=3, col="grey")
plot( gdp$time, gdp$UK, type="l", lwd=3, col="black",
      main="BVP Jungtinėje Karalystėje 2008-2020m.", sub="Duomenų šaltiniai: IHS Markit, ISM, Eurostat, skaičiavimai: Corona-stat.lt", xlab="Metai", ylab="BVP augimas, %", xlim=c(2008,2019.5))
abline(h=0, untf=FALSE, lty=3)
legend(2011,12 ,c("Gamybinis PMI","Paslaugų PMI", "Bendras PMI"),lty=c(1,1),bty = "n",lwd=3,col=c("black","steelblue", "grey"), cex = 1, horiz = TRUE)
dev.off()
