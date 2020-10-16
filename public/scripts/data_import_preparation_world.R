##  JHCSSE data import
## more info on https://github.com/CSSEGISandData/COVID-19
data_confirmed <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv', stringsAsFactors = FALSE)
data_deaths <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv', stringsAsFactors = FALSE)
data_recovered <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv', stringsAsFactors = FALSE)


# function cleaning data 
cleaning_data <- function(data) {
        data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
        data %<>% gather(key=date, value=count, -country)
        data %<>% mutate(date=date %>%substr(2,9) %>% mdy())
        data %<>% group_by(country, date) %>% summarise(count=sum(count)) %>% as.data.frame()
        
}

# applying function to clean data
data_confirmed %<>% cleaning_data()%>% rename(confirmed=count)
data_deaths %<>% cleaning_data() %>% rename(deaths=count)
data_recovered %<>% cleaning_data() %>% rename(recovered=count)


data_confirmed$country[data_confirmed$country=="US"] <- "United States of America"
data_deaths$country[data_deaths$country=="US"] <- "United States of America"
data_recovered$country[data_recovered$country=="US"] <- "United States of America"

# assembling all date in one file
# calculating active cases
data_world <- data_confirmed %>% 
        merge(data_deaths)%>% 
        merge(data_recovered)%>% 
        gather(key=var, value=value, -c(country, date)) %>%
        mutate(var=factor(var, levels=c("confirmed","deaths", "recovered"))) %>%
        mutate(valstybe=countrycode(country, origin = "country.name", destination =  "cldr.short.lt",nomatch = NULL ))%>%
        mutate(CNTR_CODE=countrycode(country, origin = "country.name", destination =  "eurostat",nomatch = NULL )) 

# saving data in subfolder for further usage
write.csv(data_world, "./data/data_world.csv", row.names = FALSE)

rm(list = ls())
