library(tidyverse)
library(rsdmx)
library(lubridate)
library(RColorBrewer)

# S8R838 - Mažmeninės prekybos, variklinių transporto priemonių, maitinimo įmonių apyvartos (be PVM) indeksai palyginamosiomis kainomis (2015 m. – 100)
# Ekonominės veiklos rūšis (EVRK 2 red.) (1998M01 - 2020M02) (Atnaujinta: 2020-04-28)
S8R838_M4070103_5 <- readSDMX(providerId = "LSD", resource = "data", flowRef = "S8R838_M4070103_5", dsd = TRUE)
S8R838_M4070103_5 <- as.data.frame(S8R838_M4070103_5 , labels = TRUE)

# S3R497 - Šalies ekonominės padėties prognozė  artimiausiems 12 mėn.
# Tendencijos kryptis | Visų gyventojų nuomonė (2001M05 - 2020M03) (Atnaujinta: 2020-03-27)
S3R497_M3230507 <- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R497_M3230507", dsd = TRUE)
S3R497_M3230507 <- as.data.frame(S3R497_M3230507 , labels = TRUE)

# S3R392 - Pinigų suma, ketinama išleisti didesniems pirkiniams (baldams, buitinei technikai) per artimiausius 12 mėn., palyginti su suma, išleista per praėjusius 12 mėn.
# Tendencijos kryptis | Visų gyventojų nuomonė (2001M05 - 2020M03) (Atnaujinta: 2020-03-27)
S3R392_M3231007 <- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R392_M3231007", dsd = TRUE)
S3R392_M3231007 <- as.data.frame(S3R392_M3231007 , labels = TRUE)


# mažmeninė apyvarta y/y
df <- S8R838_M4070103_5 %>%
        mutate(LAIKOTARPIS=as.Date(parse_date_time(LAIKOTARPIS, "y m")))%>%
        filter(EVRKM4070102 %in% c("G47", "G4711_G472", "G4719_G474_TO_G479"),
               Islyginimas_indeksai=="darbo",
               LYGINIMAS=="palyg_pm",
               LAIKOTARPIS>="2019-01-01")%>%
        mutate(EVRKM4070102_label.lt=factor(EVRKM4070102_label.lt, levels=c("Mažmeninė prekyba, išskyrus variklinių transporto priemonių ir motociklų prekybą",
                                                                            "Maisto, gėrimų ir tabako mažmeninė prekyba",
                                                                            "Mažmeninė prekyba ne maisto prekėmis")))%>%
        mutate(obsValue=round(obsValue-100,1))

png("mažmeninė_apyvarta_y_y_percent.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df, aes(LAIKOTARPIS, obsValue, fill=EVRKM4070102_label.lt))+
        geom_bar(stat="identity",
                 position = "dodge")+
        geom_text(aes(label=obsValue), position=position_dodge(width=25), vjust=-0.25, size=1.7)+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
        labs(title="Mažmeninės prekybos apyvarta, y/y, ca, %", 
             subtitle="Šaltinis: LSD Skaičiavimai: Corona-Stat.lt",
             y="Procentai", 
             x="Laikotarpis")+
        theme(plot.title = element_text(hjust = 0, face="bold"),
              legend.position = "bottom",
              legend.direction="vertical",
              legend.title=element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# mažmeninė apyvarta m/m
df <- S8R838_M4070103_5 %>%
        mutate(LAIKOTARPIS=as.Date(parse_date_time(LAIKOTARPIS, "y m")))%>%
        filter(EVRKM4070102 %in% c("G47", "G4711_G472", "G4719_G474_TO_G479"),
               Islyginimas_indeksai=="sezon",
               LYGINIMAS=="palyg_al",
               LAIKOTARPIS>="2019-01-01")%>%
        mutate(EVRKM4070102_label.lt=factor(EVRKM4070102_label.lt, levels=c("Mažmeninė prekyba, išskyrus variklinių transporto priemonių ir motociklų prekybą",
                                                                            "Maisto, gėrimų ir tabako mažmeninė prekyba",
                                                                            "Mažmeninė prekyba ne maisto prekėmis")))%>%
        mutate(obsValue=round(obsValue-100,1))

png("mažmeninė_apyvarta_m_m_percent.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df, aes(LAIKOTARPIS, obsValue, fill=EVRKM4070102_label.lt))+
        geom_bar(stat="identity",
                 position = "dodge")+
        geom_text(aes(label=obsValue), position=position_dodge(width=25), vjust=-0.25, size=1.7)+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
        labs(title="Mažmeninės prekybos apyvarta, m/m, sca, %", 
             subtitle="Šaltinis: LSD Skaičiavimai: Corona-Stat.lt",
             y="Procentai", 
             x="Laikotarpis")+
        theme(plot.title = element_text(hjust = 0, face="bold"),
              legend.position = "bottom",
              legend.direction="vertical",
              legend.title=element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# maitinimo apyvarta y/y

df <- S8R838_M4070103_5 %>%
        mutate(LAIKOTARPIS=as.Date(parse_date_time(LAIKOTARPIS, "y m")))%>%
        filter(EVRKM4070102 %in% c("I56"),
               Islyginimas_indeksai=="darbo",
               LYGINIMAS=="palyg_pm",
               LAIKOTARPIS>="2019-01-01")%>%
        mutate(obsValue=round(obsValue-100,1))

png("maitinimo_apyvarta_y_y_percent.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df, aes(LAIKOTARPIS, obsValue, fill=EVRKM4070102_label.lt))+
        geom_bar(stat="identity")+
        geom_text(aes(label=obsValue), position=position_dodge(width=25), vjust=-0.25, size=1.7)+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
        labs(title="Maitinimo ir gėrimų teikimo įmonių apyvarta, y/y, ca, %", 
             subtitle="Šaltinis: LSD Skaičiavimai: Corona-Stat.lt",
             y="Procentai", 
             x="Laikotarpis")+
        theme(plot.title = element_text(hjust = 0, face="bold"),
              legend.position = "bottom",
              legend.direction="vertical",
              legend.title=element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# maitinimo apyvarta m/m
df <- S8R838_M4070103_5 %>%
        mutate(LAIKOTARPIS=as.Date(parse_date_time(LAIKOTARPIS, "y m")))%>%
        filter(EVRKM4070102 %in% c("I56"),
               Islyginimas_indeksai=="sezon",
               LYGINIMAS=="palyg_al",
               LAIKOTARPIS>="2019-01-01")%>%
        mutate(obsValue=round(obsValue-100,1))

png("maitinimo_apyvarta_m_m_percent.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df, aes(LAIKOTARPIS, obsValue, fill=EVRKM4070102_label.lt))+
        geom_bar(stat="identity")+
        geom_text(aes(label=obsValue), position=position_dodge(width=25), vjust=-0.25, size=1.7)+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
        labs(title="Maitinimo ir gėrimų teikimo įmonių apyvarta, m/m, sca, %", 
             subtitle="Šaltinis: LSD Skaičiavimai: Corona-Stat.lt",
             y="Procentai", 
             x="Laikotarpis")+
        theme(plot.title = element_text(hjust = 0, face="bold"),
              legend.position = "bottom",
              legend.direction="vertical",
              legend.title=element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()




# mažmeinė + maitinimo apyvarta y/y ts

df <- S8R838_M4070103_5 %>%
        mutate(LAIKOTARPIS=as.Date(parse_date_time(LAIKOTARPIS, "y m")))%>%
        filter(EVRKM4070102 %in% c("G47", "G4711_G472", "G4719_G474_TO_G479", "I56"),
               Islyginimas_indeksai=="darbo",
               LYGINIMAS=="palyg_pm",
               LAIKOTARPIS>="2008-01-01")%>%
        mutate(EVRKM4070102_label.lt=factor(EVRKM4070102_label.lt, levels=c("Mažmeninė prekyba, išskyrus variklinių transporto priemonių ir motociklų prekybą",
                                                                            "Maisto, gėrimų ir tabako mažmeninė prekyba",
                                                                            "Mažmeninė prekyba ne maisto prekėmis",
                                                                            "Maitinimo ir gėrimų teikimo veikla")))%>%
        mutate(obsValue=round(obsValue-100,1))

png("apyvarta_y_y_percent_ts.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df, aes(LAIKOTARPIS, obsValue, col=EVRKM4070102_label.lt))+
        geom_line()+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m")+
        labs(title="Mažmeninės prekybos, maitinimo ir \ngėrimų teikimo veiklos a, y/y, ca, %", 
             subtitle="Šaltinis: LSD Skaičiavimai: Corona-Stat.lt",
             y="Procentai", 
             x="Laikotarpis")+
        theme(plot.title = element_text(hjust = 0, face="bold"),
              legend.position = "bottom",
              legend.direction="vertical",
              legend.title=element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()



# mažmeinė + maitinimo apyvarta m/m ts
df <- S8R838_M4070103_5 %>%
        mutate(LAIKOTARPIS=as.Date(parse_date_time(LAIKOTARPIS, "y m")))%>%
        filter(EVRKM4070102 %in% c("G47", "G4711_G472", "G4719_G474_TO_G479", "I56"),
               Islyginimas_indeksai=="sezon",
               LYGINIMAS=="palyg_al",
               LAIKOTARPIS>="2008-01-01")%>%
        mutate(EVRKM4070102_label.lt=factor(EVRKM4070102_label.lt, levels=c("Mažmeninė prekyba, išskyrus variklinių transporto priemonių ir motociklų prekybą",
                                                                            "Maisto, gėrimų ir tabako mažmeninė prekyba",
                                                                            "Mažmeninė prekyba ne maisto prekėmis",
                                                                            "Maitinimo ir gėrimų teikimo veikla")))%>%
        mutate(obsValue=round(obsValue-100,1))

png("apyvarta_m_m_percent_ts.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df, aes(LAIKOTARPIS, obsValue, col=EVRKM4070102_label.lt))+
        geom_line()+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m")+
        labs(title="Mažmeninės prekybos, maitinimo ir \ngėrimų teikimo veiklos apyvarta, m/m, sca, %", 
             subtitle="Šaltinis: LSD Skaičiavimai: Corona-Stat.lt",
             y="Procentai", 
             x="Laikotarpis")+
        theme(plot.title = element_text(hjust = 0, face="bold"),
              legend.position = "bottom",
              legend.direction="vertical",
              legend.title=element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()




# lūkesčiai ekonomika t+12

df1 <- S3R497_M3230507%>%
        mutate(LAIKOTARPIS=as.Date(parse_date_time(LAIKOTARPIS, "y m")))%>%
        filter(tendencijaM3230301=="bal")%>%
        select(LAIKOTARPIS, obsValue)%>%
        rename("Šalies ekonominės padėties prognozė artimiausiems 12 mėn, balansas"=obsValue)

df2 <- S3R392_M3231007%>%
        mutate(LAIKOTARPIS=as.Date(parse_date_time(LAIKOTARPIS, "y m")))%>%
        filter(tendencijaM3231001=="bal")%>%
        select(LAIKOTARPIS, obsValue)%>%
        rename("Pinigų suma, ketinama išleisti didesniems pirkiniams per artimiausius 12 mėn., balansas"=obsValue)

df <- merge(df1, df2)%>%gather(var, values, 2:3)

png("vartotoju_lukesciai_ts.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df, aes(LAIKOTARPIS, values, col=var))+
        geom_line()+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m")+
        labs(title="Vartotojų lūkesčiai, balansas", 
             subtitle="Šaltinis: LSD Skaičiavimai: Corona-Stat.lt",
             y="Procentai", 
             x="Laikotarpis")+
        theme(plot.title = element_text(hjust = 0, face="bold"),
              legend.position = "bottom",
              legend.direction="vertical",
              legend.title=element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

write.csv(df, "df.csv")
        