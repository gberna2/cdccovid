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
g$prob_cases=as.numeric(g$prob_cases)

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
    geom_line()+
    theme_classic()

# new deaths
ggplot(ma, aes(date, new_death))+
    geom_line()+
    theme_classic()


# ma new cases and probable cases

manewprob<- g%>%
    filter(state=="MA") %>%
    select(submission_date, state, new_case, prob_cases)%>%
    na.omit()%>%
    filter(as.Date(submission_date)>"2020-08-01")%>%
    filter(new_case>0)

attach(manewprob)

ggplot(manewprob, aes(x=submission_date))+
           geom_line(aes(y=new_case), color="blue")+
    geom_line(aes(y=prob_cases), color="red")+
    theme_classic()

view(manewprob)
