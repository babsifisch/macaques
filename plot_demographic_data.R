library("tidyverse")
library("janitor")
library("readxl")
library("forcats")
library("ggplot2")
library("patchwork")

#import data

data_year<-read_csv2("Daten_Barbara_Jahresstatistik.csv",col_names=TRUE)%>%
  clean_names()

data_month<-read_csv2("Daten_Barbara_Monatsstatistik.csv",col_names=TRUE)%>%
  clean_names()
data_month$month<-c("January","Feburary","March","April","May","June","July","August","September","October","November","December") %>% as_factor()

#the following code produces histograms, one per variable (column)

birthsdistri<-data_month %>%
ggplot(aes(month,births_total))+
  geom_col(fill="darkmagenta") +
  labs( 
       y = "births")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

deathsinfantdistri<-data_month %>%
  ggplot(aes(month,death_infant))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(0,5,10),limits=c(0,12),"deaths infants")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

deathsfdistri<-data_month %>%
  ggplot(aes(month,death_f))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(0,5,10),limits=c(0,12),"deaths females")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

deathsfmaturedistri<-data_month %>%
  ggplot(aes(month,sexm_f_death))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(0,5,10),limits=c(0,12),"deaths mature females")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#produce a joint plot, save it

birthsdistri/plot_spacer()/deathsinfantdistri/deathsfmaturedistri/deathsfdistri+plot_layout(heights = c(4, 1 ,4,4,4))

ggsave("birth_death_distributions.pdf", width = 20, height = 20, units = "cm")

#now produce a plot with more subgroups

deathsinfantdistri<-data_month %>%
  ggplot(aes(month,death_infant))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(0,5,10,15),limits=c(0,18),"deaths infants")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

deathsfdistri<-data_month %>%
  ggplot(aes(month,death_f))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(breaks=c(0,5,10,15),limits=c(0,18),"deaths females")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

deathsfimmaturedistri<-data_month %>%
  ggplot(aes(month,imm_f_death))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(0,5,10,15),limits=c(0,18),"deaths immature females")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

deathsfmaturedistri<-data_month %>%
  ggplot(aes(month,sexm_f_death))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(0,5,10,15),limits=c(0,18),"deaths mature females")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

deathsfmaturesterildistri<-data_month %>%
  ggplot(aes(month,sexm_steril_f_death))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(0,5,10,15),limits=c(0,18),"deaths sterilized females")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

deathsdistri<-data_month %>%
  ggplot(aes(month,deaths_total))+
  geom_col(fill="azure3") +
  scale_y_continuous(breaks=c(0,5,10,15),limits=c(0,18),"all deaths")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

birthsdistri/plot_spacer()/deathsinfantdistri/deathsfmaturedistri/deathsfmaturesterildistri/deathsfdistri/deathsdistri+plot_layout(heights = c(4,1,4,4,4,4,4))

ggsave("birth_death_distributions_moresubgroups.pdf", width = 20, height = 35, units = "cm")
