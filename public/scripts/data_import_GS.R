# data import from google sheets - LITHUANIA
URL <- "https://docs.google.com/spreadsheets/d/1l9HM2hIjLFA-SZy_kO8b8B5Ddr0hfVZrmx2Yb1n1feg/edit#gid=756226331"
data_lt <- read.csv(text=gsheet2text(URL, format = "csv"),
                    stringsAsFactors=FALSE,
                    na.strings = "")%>%
        select(1:13)%>%
        mutate(date=as.Date(date))

# writing csv
write.csv(data_lt, "./data/data_lt.csv", row.names = FALSE)


# data import from google sheets - LATVIA
URL <- "https://docs.google.com/spreadsheets/d/1l9HM2hIjLFA-SZy_kO8b8B5Ddr0hfVZrmx2Yb1n1feg/edit#gid=2057992342"
data_lv <- read.csv(text=gsheet2text(URL, format = "csv"),
                    stringsAsFactors=FALSE,
                    na.strings = "")%>%
        select(1:13)%>%
        mutate(date=as.Date(date)) 

# writing csv
write.csv(data_lv, "./data/data_lv.csv", row.names = FALSE)


# data import from google sheets - ESTONIA
URL <- "https://docs.google.com/spreadsheets/d/1l9HM2hIjLFA-SZy_kO8b8B5Ddr0hfVZrmx2Yb1n1feg/edit#gid=899246534"
data_ee <- read.csv(text=gsheet2text(URL, format = "csv"),
                    stringsAsFactors=FALSE,
                    na.strings = "")%>%
        select(1:13)%>%
        mutate(date=as.Date(date)) 

# writing csv
write.csv(data_ee, "./data/data_ee.csv", row.names = FALSE)


rm(list = ls())
