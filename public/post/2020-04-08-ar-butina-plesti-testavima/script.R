library(tidyverse)
library(eurostat)

data_lt <- read.csv("./content/data/data_lt.csv",header = TRUE,stringsAsFactors = FALSE)%>%
        mutate(date=as.Date(date))

data_lt_country <- read.csv("./content/data/data_lt_country.csv",header = TRUE,stringsAsFactors = FALSE)%>%
        mutate(date=as.Date(date),
               var=factor(var, levels=c("confirmed","active", "recovered", "deaths")))

data_lv <- read.csv("./content/data/data_lv.csv",header = TRUE,stringsAsFactors = FALSE)%>%
        mutate(date=as.Date(date))

data_ee <- read.csv("./content/data/data_ee.csv",header = TRUE,stringsAsFactors = FALSE)%>%
        mutate(date=as.Date(date))

demo_r_pjangrp <- get_eurostat("demo_r_pjangrp3", 
                               stringsAsFactors = FALSE, 
                               filters = list(age="TOTAL",
                                              sex="T",
                                              time="2019"))%>%
        rename(NUTS_ID=geo,
               pop=values)

#### first graph

pop <- demo_r_pjangrp%>%
        filter(NUTS_ID %in% c("LT", "LV", "EE"))%>%
        select(NUTS_ID, pop)%>%
        spread(NUTS_ID,pop)

lt <- data_lt %>%
        select(date, tested_total)%>%
        rename(tested_total_lt=tested_total)

lv <- data_lv %>%
        select(date, tested_total)%>%
        rename(tested_total_lv=tested_total)

ee <- data_ee %>%
        select(date, tested_total)%>%
        rename(tested_total_ee=tested_total)

df <- merge(lt,lv)%>% merge(.,ee) %>%
        mutate(
                tested_total_lt_p=tested_total_lt/pop$LT*1000,
                tested_total_lv_p=tested_total_lv/pop$LV*1000,
                tested_total_ee_p=tested_total_ee/pop$EE*1000
        )%>%
        gather(var, values, 2:7)%>%
        mutate(case=rep(c("Skaičius", "Skaičius tenkantis 1000 gyventojų"), each=nrow(.)/2),
               geo=rep(c("Lietuva", "Latvija", "Estija"), each=nrow(lt), times=2))





png("./static/post/2020-04-08-ar-butina-plesti-testavima_files/2020-04-08-tested-number.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df, aes(date, values, col=geo))+
        geom_line(size=1.1)+
        geom_point(size=2)+
        scale_color_brewer(palette="Set1",type = "qualitative", name="Valstybės")+
        labs(
                title="Testavimo apimtys Baltijos šalyse",
                subtitle="Šaltinis ir skaičiavimai: Corona-Stat.lt",
                x="Laikotarpis",
                y="Skaičius"
        )+
        scale_x_date(breaks = "1 week")+
        theme(legend.position = "bottom",
              axis.text=element_text(size=8),
              axis.text.x=element_text(angle=45, hjust=1))+
        facet_wrap(~case, scales = "free_y")
dev.off()


#### second graph

lt_1 <- data_lt %>%
        select(date, tested_new)%>%
        rename(tested_new_lt=tested_new)

lt_2 <- data_lt_country %>%
        filter(var=="confirmed")%>%
        select(date, values)%>%
        rename(conf_new_lt=values)

lv <- data_lv %>%
        select(date, conf_new ,tested_new)%>%
        rename(conf_new_lv=conf_new,
               tested_new_lv=tested_new)

ee <- data_ee %>%
        select(date,conf_new, tested_new)%>%
        rename(conf_new_ee=conf_new,
               tested_new_ee=tested_new)

df <- merge(lt_1,lt_2) %>% merge(.,lv) %>% merge(.,ee)%>%
        mutate(
                Lietuva=conf_new_lt/tested_new_lt*100,
                Latvija=conf_new_lv/tested_new_lv*100,
                Estija=conf_new_ee/tested_new_ee*100
        )%>%
        select(1,8,9,10)%>%
        gather(geo, values, 2:4)


png("./static/post/2020-04-08-ar-butina-plesti-testavima_files/2020-04-08-positive-to-total.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(subset(df, geo!="Estija"), aes(date, values, col=geo))+
        geom_line(size=1.1)+
        geom_point(size=2)+
        scale_y_continuous(breaks = seq(0,30,2))+
        scale_color_brewer(palette="Set1",type = "qualitative", name="Valstybės")+
        labs(
                title="Santykis tarp aptiktų atvejų ir atliktų testų",
                subtitle="Šaltinis ir skaičiavimai: Corona-Stat.lt",
                x="Laikotarpis",
                y="Santykis"
        )+
        geom_hline(yintercept = 10, col="blue", linetype=2)+
        geom_hline(yintercept = 2, col="red", linetype=2)+
        scale_x_date(breaks = "2 day")+
        theme(legend.position = "bottom",
              axis.text=element_text(size=8),
              axis.text.x=element_text(angle=45, hjust=1))
dev.off()
