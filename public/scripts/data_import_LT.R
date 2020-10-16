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