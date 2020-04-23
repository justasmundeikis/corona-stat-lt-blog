
if(!require("tidyverse")) install.packages("tidyverse", dep = T); library("tidyverse")

nafta <- read.csv("nafta.csv")

nafta <- na.omit(nafta)

names(nafta)[1] <- "Data"
names(nafta)[2] <- "WTI"
names(nafta)[3] <- "BRENT"

nafta$Data <- as.Date(nafta$Data)

colors <- c("BRENT" = "blue", "WTI" = "red")

png("./figures/naftos_kainos_WTI_BRENT.png", width = 9, height = 4, units = 'in', res = 200)
ggplot(nafta,aes(x=Data))+
        geom_line(aes(y=WTI,color="WTI"),size=0.7)+
        geom_line(aes(y=BRENT,color="BRENT"),size=0.7)+
        scale_x_date(breaks="1 week")+
        labs(title="Naftos kainos",
             subtitle="Šaltinis:eia.gov, skaičiavimai: Corona-Stat.lt",
             x="Laikotarpis",
             y="Kaina",
             color="Legend")+
        scale_color_manual(values = colors)+
        theme(legend.position="right",
              axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()


        

