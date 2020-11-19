library(tidyverse)
library(ggthemes)

#read in data from cdc 
conditions<-read.socrata("https://data.cdc.gov/resource/hk9y-quqm.json", app_token = "du1xYluz5Q3hWLPoeoxced3Dg", stringsAsFactors = FALSE)
#convert to numeric
conditions$number_covid19_deaths=as.numeric(conditions$number_covid19_deaths)

attach(conditions)
view(conditions)
view(mass)
#remove columns
conditions%>%
    select(data_as_of, state, condition_group, condition, age_group, number_covid19_deaths)%>%
    na.omit()

mass<- conditions %>%
    filter(state== "MA")


age<- mass %>%
    replace_na(list(number_covid19_deaths= 0))%>%
    group_by(age_group) %>%
    summarise(deaths=sum(number_covid19_deaths)) %>%
    filter(age_group != c("All Ages", "Not stated"))

# MA death by condition group

mc_group<- mass %>%
    drop_na("number_covid19_deaths")%>%
    group_by(condition_group)%>%
    summarise(deaths= sum(number_covid19_deaths))%>%
    mutate(percent=(deaths/sum(deaths)*100))%>%
    mutate(percent= round(percent, 0))%>%
    filter(percent>0)%>%
    arrange(desc(percent))
    mc_group$condition_group[mc_group$condition_group=="COVID-19"]<-"Covid"
    mc_group$condition_group[mc_group$condition_group=="Intentional and unintentional injury, poisoning, and other adverse events"]<-"Injury"
    mc_group$condition_group[mc_group$condition_group=="All other conditions and causes (residual)"]<-"Other"
    mc_group$condition_group[mc_group$condition_group=="Vascular and unspecified dementia"]<-"Vascular"
    mc_group$condition_group[mc_group$condition_group=="Malignant neoplasms"]<-"Neoplasm"
    mc_group$condition_group=as.factor(mc_group$condition_group)
    
attach(mc_group)

ggplot(mc_group, aes(x = reorder(condition_group, percent), percent))+
    geom_bar(stat="identity")+
    theme(axis.text = element_text(angle=90))+
    labs(title = "MA Percent of Deaths by Reason")+
    xlab("Reason")+ ylab("percent of deaths")
    

