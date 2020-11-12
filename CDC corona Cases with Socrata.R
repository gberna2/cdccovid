#packages
library(RSocrata)
library(tidyverse)
library(lubridate)
library(data.table)

# this is the app token
toke<-du1xYluz5Q3hWLPoeoxced3Dg
# this is the socrata connection
g<-read.socrata("https://data.cdc.gov/resource/9mfq-cb36.json", app_token = "du1xYluz5Q3hWLPoeoxced3Dg", stringsAsFactors = FALSE)
# data is imported as characters following syntax converst to numeric
g$tot_death=as.numeric(g$tot_death)
g$new_death=as.numeric(g$new_death)
g$new_case=as.numeric(g$new_case)

# massachusetts total deaths
ma<- g %>%
    filter(state=="MA") %>%
    filter(as.Date(submission_date)>"2020-05-22")%>%
    select(submission_date, tot_death, new_case, new_death, ) %>%
    rename("total_deaths"=tot_death, "date"= submission_date)

totaldeathplot<-ggplot(ma, aes(date, total_deaths))+
    geom_line() 

# massachusetts new cases
caseplot<-ggplot(ma, aes(date, new_case))+
    geom_line()

# new deaths
ggplot(ma, aes(date, new_death))+
    geom_line()

view(ma)

attach(ma)

ggplot(ma, aes(submission_date, tot_death))+
    geom_line()

view(ma)

b<- 21.23

view(test)

class(new_case)

bi<- g %>%
    select(tot_death)
i<-as.numeric(unlist(bi))

o<- ma %>%
    as.numeric(unlist(tot_death))

               
ggplot(td, aes(submission_date, tot_death))+
    geom_point()

library()
getwd()



