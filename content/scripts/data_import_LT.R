## importing NVSC data (case-data)
data_import <- readLines("ftp://atviriduomenys.nvsc.lt/COVID19.json")
data_lt <- fromJSON(data_import, flatten = TRUE)%>%
        rename(date_infection=`Susirgimo data`,
               date_confirmation=`Atvejo patvirtinimo data`,
               imported=Įvežtinis,
               imported_country=Šalis,
               result=Išeitis,
               foreigner=Užsienietis,
               age_group=`Atvejo amžius`,
               sex=Lytis,
               county=Savivaldybė,
               is_hospitalized=`Ar hospitalizuotas`,
               is_ICU=`Gydomas intensyvioje terapijoje`,
               has_chronic=`Turi lėtinių ligų`)%>% 
        mutate(date_infection=as.Date(date_infection),
               date_confirmation=as.Date(date_confirmation))

data_lt$is_ICU[data_lt$is_ICU==""] <- "Ne"
data_lt$has_chronic[data_lt$has_chronic==""] <- "Ne"
data_lt$imported_country[data_lt$imported_country==""] <- NA
data_lt$age_group[data_lt$age_group==""] <- NA
data_lt$sex[data_lt$sex==""] <- NA

# writing csv
write.csv(data_lt, "./data/data_lt.csv", row.names = FALSE)


## importing testing data
#### data_lt_test <- read.csv("https://opendata.arcgis.com/datasets/538b7bd574594daa86fefd16509cbc36_0.csv")
data_import <- readLines("https://opendata.arcgis.com/datasets/07dce7d43ba04a5b93abbbbe1d20d9ea_0.geojson")
data_import <- fromJSON(data_import, flatten = TRUE)
data_lt_test <- as.data.frame(data_import$features)%>%
        select(2:12)
names(data_lt_test)<- gsub("\\properties.", "", names(data_lt_test))
data_lt_test$test_performed_date <- as.Date(data_lt_test$test_performed_date)

# writing csv
write.csv(data_lt_test, "./data/data_lt_test.csv", row.names = FALSE)


rm(list = ls())
