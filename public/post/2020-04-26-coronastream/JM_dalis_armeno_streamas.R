if(!require("tidyverse")) install.packages("tidyverse", dep = T); library("tidyverse")
if(!require("eurostat")) install.packages("eurostat", dep = T); library("eurostat")
if(!require("lubridate")) install.packages("lubridate", dep = T); library("lubridate")
if(!require("RColorBrewer")) install.packages("RColorBrewer", dep = T); library("RColorBrewer")
if(!require("zoo")) install.packages("zoo", dep = T); library("zoo")
if(!require("kableExtra")) install.packages("kableExtra", dep = T); library("kableExtra")
if(!require("rvest")) install.packages("rvest", dep = T); library("rvest")
if(!require("magrittr")) install.packages("magrittr", dep = T); library("magrittr")

# creating empty folder for figures
if(!dir.exists("figures")) dir.create("figures")

# downloading data
prc_hicp_manr <- get_eurostat("prc_hicp_manr", stringsAsFactors = FALSE)
UT_data <-read.csv("vidurkis_datai_data-1.csv", stringsAsFactors = FALSE, sep=";")
apple <- read.csv("./applemobilitytrends-2020-04-22.csv", stringsAsFactors = FALSE)
        

################################################################################
#### Eurostat - inflation
################################################################################
# overall - groups
################################################################################
df <- prc_hicp_manr %>% filter(coicop %in% c("CP00", "FOOD","IGD_NNRG", "NRG", "SERV" ),
                               geo=="EA",
                               year(time)>=2010)

png("./figures/infliacija_ez.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(df, aes(time, values, col=coicop))+
        geom_line(size=1.1)+
        scale_x_date(breaks="1 year", date_labels = "%Y-%b")+
        scale_color_brewer(palette = "Set1", type = "qual")+
        labs(title="Eurozonos metinė infliacija ir jos grupės",
             subtitle="Šaltinis: Eurostat, skaičiavimai: Corona-Stat.lt",
             x="Laikotarpis",
             y="Pokytis, %")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")

dev.off()

################################################################################
# food
################################################################################
df <- prc_hicp_manr %>% filter(coicop %in% c("CP011" ),
                               geo %in% ea_countries$code,
                               time==max(time))
date <- max(df$time)

png("./figures/infliacija_maistas.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(df, aes(reorder(geo, values), values))+
        geom_bar(stat="identity",
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5)+
        labs(title=paste0("Eurozonos šalių maisto metinė infliacija ", year(date),"-",month(date, label = TRUE, abbr = FALSE)),
             subtitle="Šaltinis: Eurostat, skaičiavimai: Corona-Stat.lt",
             x="Šalys",
             y="Pokytis, %")
dev.off()

################################################################################
# elektra, dujos, kuras
################################################################################
df <- prc_hicp_manr %>% filter(coicop %in% c("CP045" ),
                               geo %in% ea_countries$code,
                               time==max(time))

date <- max(df$time)
png("./figures/infliacija_elektra_dujos.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(df, aes(reorder(geo, values), values))+
        geom_bar(stat="identity",
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5)+
        labs(title=paste0("Elektros, dujų ir kito kuro metinė infliacija ", year(date),"-",month(date, label = TRUE, abbr = FALSE)),
             subtitle="Šaltinis: Eurostat, skaičiavimai: Corona-Stat.lt",
             x="Šalys",
             y="Pokytis, %")
dev.off()

################################################################################
# degalai
################################################################################
df <- prc_hicp_manr %>% filter(coicop %in% c("CP0722" ),
                               geo %in% ea_countries$code,
                               time==max(time))
date <- max(df$time)
png("./figures/infliacija_degalai.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(df, aes(reorder(geo, values), values))+
        geom_bar(stat="identity",
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5)+
        labs(title=paste0("Asmeninių transporto priemonių degalų ir tepalų metinė infliacija ", year(date),"-",month(date, label = TRUE, abbr = FALSE)),
             subtitle="Šaltinis: Eurostat, skaičiavimai: Corona-Stat.lt",
             x="Šalys",
             y="Pokytis, %")
dev.off()


################################################################################
# UT Grupiniai atleidimai
################################################################################
url <- "https://uzt.lt/darbo-rinka/grupes-darbuotoju-atleidimai-2/"
html <- read_html(url, encoding = "utf-8")
df <- html_nodes(html, xpath="//td") %>% html_text()%>%trimws()
df[grepl("iki 117", df)] <- 117
df[grepl("173 ", df)] <- 173

df <- df%>%
        matrix(., ncol=4, byrow=T)%>%
        data.frame(stringsAsFactors = FALSE)%>%
        rename(company=X1,
               sector=X2,
               date=X3,
               values=X4)%>%
        mutate(date=as.Date(date),
               yrmn=as.factor(as.yearmon(date)),
               values=as.numeric(values))


df1 <- df %>%
        filter(date>="2020-03-01")%>%
        group_by(sector)%>%
        summarise(values=sum(values))%>%
        arrange(desc(values))%>%
        top_n(10)%>%
        rename(Sektorius=sector,
               Skaičius=values)

writeLines(
        kable(df1, format = "latex", booktabs=T)%>%
                kable_styling(latex_options =c("striped", "scale_down")), "grupiniai_atleidimai.txt")

################################################################################
# grupiniai atleidimai skaičius
################################################################################
df2 <- df %>% group_by(yrmn)%>%
        summarise(values=sum(values))
png("./figures/grupes_darbuotoju_atleidimai_men_sum.png", width = 9, height = 4, units = 'in', res = 200)
ggplot(df2, aes(yrmn, values))+
        geom_bar(stat="identity",
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5, size=3)+
        labs(title="Grupės darbuotojų atleidimai",
             subtitle="Šaltinis: Užimtumo tarnyba, skaičiavimai: Corona-Stat.lt",
             x="Laikotarpis",
             y="Skaičius")
dev.off()



################################################################################
#### UT
################################################################################
df <- UT_data %>%
        mutate(Laikas=as.Date(Laikas))%>%
        filter(Rodiklis %in% c(#"bedarbių sk. laikotarpio pabaigoje", 
                "bedarbių vyrų sk. laikotarpio pab.", 
                "bedarbių moterų sk. laikotarpio pab." ),
               Laikas>="2016-10-01")

png("./figures/bedarbiu_sk.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(df, aes(Laikas,reikšmė, col=Rodiklis))+
        geom_line()+
        geom_point()+
        scale_x_date(breaks="2 months", date_labels = "%Y-%b")+
        scale_color_brewer(palette = "Set1", type = "qual")+
        labs(title="Bedarbių skaičius (laikotarpio pabaigai)",
             subtitle="Šaltinis: Užimtumo tarnyba, skaičiavimai: Corona-Stat.lt",
             x="Laikotarpis",
             y="Skaičius")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
dev.off()

################################################################################
df <- UT_data %>%
        mutate(Laikas=as.Date(Laikas))%>%
        filter(Rodiklis %in% c("bedarbių proc. nuo DAG", 
                               "bedarbių vyrų proc. nuo DA vyrų", 
                               "bedarbių moterų proc. nuo DA moterų" ),
               Laikas>="2017-01-01")

png("./figures/bedarbiu_proc.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(df, aes(Laikas, reikšmė, col=Rodiklis))+
        geom_line()+
        scale_x_date(breaks="2 months", date_labels = "%Y-%b")+
        scale_color_brewer(palette = "Set1", type = "qual")+
        labs(title="Bedarbių proc. nuo DAG (laikotarpio pabaigai), %",
             subtitle="Šaltinis: Užimtumo tarnyba, skaičiavimai: Corona-Stat.lt",
             x="Laikotarpis",
             y="%")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
dev.off()

##############################################                  ##################################
#### Apple
################################################################################
#### Pasirinktos šalys
################################################################################
apple <- apple %>%
        gather(key=date, value=values, 4:ncol(.))%>%
        mutate(date=date %>% substr(2,11) %>% ymd())%>%
        rename(geo=region,
               var=transportation_type)

df <- apple%>%
        filter(geo %in% c("Lithuania", "Latvia", "Estonia", "Sweden", "Germany"))%>%
        group_by(geo, var)%>%
        mutate(values=rollmean(values,7, na.pad = TRUE))

png("./figures/apple_judejimas.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(df, aes(date, values, col=geo))+
        geom_line(size=1.2)+
        scale_color_brewer(palette = "Set1", type = "qual")+
        facet_grid(cols=vars(var))+
        labs(title="Apple maps užklausų skaičiaus indeksas (7d. slenkantis vidurkis)",
             subtitle="Šaltinis: Apple.com, skaičiavimai: Corona-Stat.lt",
             x="Laikotarpis",
             y="")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")

dev.off()

################################################################################
#### Lietuva
################################################################################
df <- apple%>%
        filter(geo=="Lithuania")%>%
        group_by(geo, var)

png("./figures/apple_judejimas_lt.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(df, aes(date, values, col=var))+
        geom_line(size=1.2)+
        scale_color_brewer(palette = "Set1", type = "qual")+
        scale_x_date(breaks="1 weeks")+
        labs(title="Apple maps užklausų skaičiaus indeksas Lietuvoje",
             subtitle="Šaltinis: Apple.com, skaičiavimai: Corona-Stat.lt",
             x="Laikotarpis",
             y="")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")

dev.off()
