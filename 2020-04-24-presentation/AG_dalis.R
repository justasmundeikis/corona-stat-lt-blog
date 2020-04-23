
if(!require("tidyverse")) install.packages("tidyverse", dep = T); library("tidyverse")

#daily
nafta <- read.csv("nafta.csv")

names(nafta)[1] <- "Data"
names(nafta)[2] <- "WTI"
names(nafta)[3] <- "BRENT"

nafta$Data <- as.Date(nafta$Data)
nafta <- filter(nafta,Data>="2019-05-01")

colors <- c("BRENT" = "blue", "WTI" = "red")

png("./figures/naftos_kainos_daily.png", width = 9, height = 4, units = 'in', res = 200)

ggplot(nafta,aes(x=Data))+
        geom_line(aes(y=WTI,color="WTI"),size=0.7)+
        geom_line(aes(y=BRENT,color="BRENT"),size=0.7)+
        scale_x_date(breaks="1 month")+
        labs(title="Naftos kainos",
             subtitle="Šaltinis: eia.gov, skaičiavimai: Corona-Stat.lt",
             x="Laikotarpis",
             y="Kaina",
  color="Naftos tipas")+
  scale_color_manual(values = colors)+
  theme(legend.position="right",
  axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept=0)

dev.off()

#monthly, index

nafta2 <- read.csv("IMF.csv")

names(nafta2)[1] <- "Data"
nafta2$Data <- as.Date(nafta2$Data)

nafta2 <- mutate(nafta2,Nafta=Nafta/Nafta[1]*100)
nafta2 <- mutate(nafta2,Dujos=Dujos/Dujos[1]*100)

colors <- c("Nafta" = "blue", "Dujos" = "red")

png("./figures/naftos_kainos_monthly.png", width = 9, height = 4, units = 'in', res = 200)

ggplot(nafta2,aes(x=Data))+
  geom_line(aes(y=Nafta,color="Nafta"),size=0.7)+
  geom_line(aes(y=Dujos,color="Dujos"),size=0.7)+
  scale_x_date(date_labels = "%Y",breaks="1 year")+
  labs(title="Naftos ir dujų kainų indeksai, 1992 = 100",
       subtitle="Šaltinis:imf.org, skaičiavimai: Corona-Stat.lt",
       x="Laikotarpis",
       y="Indeksas",
       color="Žaliava")+
  scale_color_manual(values = colors)+
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()       

