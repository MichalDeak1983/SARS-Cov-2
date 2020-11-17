# Plotting the correlation plot between variables of choice
# Input: coronaV data frame scraped by the covid19.R code
# Parameters:	delay - delay between cases and deaths
#		start - starting day
#		end - ending day
#		country
#		av - number of days to average over

# Loading the libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rvest)

# Setting the values of the parameters
delay<-9
start<-2
end<-194 # max 205
country<-"USA"
av<-7

# Vectors to store various variables
# Length depends on the parameters delay, start and end
new_cases <- vector(mode="character", length=(end-delay-start+1))
new_deaths <- vector(mode="character", length=(end-delay-start+1))
death_rate <- vector(mode="character", length=(end-delay-start+1))
new_tests <- vector(mode="character", length=(end-delay-start+1))

# Looping over the days, deaths related quantities
# Starting at delay + start
for(i in (delay+1):(end+delay-start+2)) {

cnod<-coronaV[[i+start-1]] %>% filter_all(any_vars(. %in% country))
cnod2<-coronaV[[i+start-2]] %>% filter_all(any_vars(. %in% country))
cd<-coronaV[[i+start-1]] %>% filter_all(any_vars(. %in% country))

di<-dim(cnod)[1]

# Storing 'NewDeaths' quantity in newdeaths vector
new_deaths[i-delay]<-cd[di,'NewDeaths']

a1<-as.numeric(gsub(",","",cnod[di,'TotalDeaths']))
a2<-as.numeric(gsub(",","",cnod[di,'TotalRecovered']))
b1<-as.numeric(gsub(",","",cnod[di,'TotalRecovered']))
b2<-as.numeric(gsub(",","",cnod2[di,'TotalRecovered']))

# Storing the death rate quantity in deathrate vector
death_rate[i-delay]<-(a1/(a1+a2))

}

# Looping over the days, new cases
# Starting at start
for(i in 1:(end-start+1)) {

cd<-coronaV[[i+start-1]] %>% filter_all(any_vars(. %in% country))

di<-dim(cd)[1]

newcases[i]<-cd[di,'NewCases']

}

# Vectors to store various averaged variables
# Length depends on the parameters delay, start and end
newcasesA <- vector(mode="character", length=(end-start+1-av+1))
newdeathsA <- vector(mode="character", length=(end-start+1-av+1))
deathrateA <- vector(mode="character", length=(end-start+1-av+1))

# Lower f_s and upper f_e bound for averaging
f_s<-floor(av/2.)
f_e<-ceiling(av/2.)-1

for(i in (f_s+1):(end-start+1-f_e)) {
newcasesA[i-f_s] <- mean(as.numeric(gsub(",","",newcases[(i-f_s):(i+f_e)])),na.rm=TRUE)
newdeathsA[i-f_s] <- mean(as.numeric(gsub(",","",newdeaths[(i-f_s):(i+f_e)])),na.rm=TRUE)
deathrateA[i-f_s] <- mean(as.numeric(gsub(",","",deathrate[(i-f_s):(i+f_e)])),na.rm=TRUE)
}

fc<-start:(end-av+1)

newcasesNum<-as.numeric(gsub(",","",newcasesA))
a <- data.frame(fc,newcasesNum)
ggplot(data=a, aes(x=fc, y=newcasesNum, group=1)) + labs(x="Day",y="New Cases",title=str_c("New cases, ",country,sep="",collapse=NULL)) + geom_line()+ geom_point()

deathrateNum<-as.numeric(gsub(",","",deathrateA))
aa <- data.frame(fc,deathrateNum)
ggplot(data=aa, aes(x=fc, y=deathrateNum, group=1)) + geom_line() + geom_point() + scale_y_continuous(trans='log10')

newdeathsNum<-as.numeric(gsub(",","",newdeathsA))
aaa <- data.frame(fc,newdeathsNum)
ggplot(data=aaa, aes(x=fc, y=newdeathsNum, group=1)) + labs(x="Day",y="New Deaths",title=str_c("New Deaths, ",country,sep="",collapse=NULL)) + geom_line() + geom_point()
#+ scale_y_continuous(trans='log10')

#newtestsNum<-as.numeric(gsub(",","",newtests))
#aaaa <- data.frame(fc,newtestsNum)
#ggplot(data=aaaa, aes(x=fc, y=newtestsNum, group=1)) + geom_line() + geom_point() 
#+ scale_y_continuous(trans='log10')

ffc<-ifelse(fc>12,ifelse(fc>=80,"2nd","1st"),"Outliers")
b <- data.frame(ffc,as.numeric(newdeathsNum),as.numeric(newcasesNum))
b$ffc<-as.factor(b$ffc)

mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5), 
                 legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"), 
                 legend.text = element_text(face = "italic", colour="black",family = "Helvetica"), 
                  axis.title = element_text(family = "Helvetica", size = (10), colour = "black"),
                  axis.text = element_text(family = "Helvetica", colour = "black", size = (10)))

png(str_c("av",av,"-",country,"noav-del",delay,"-full.png",sep="",collapse=NULL))

plot<-ggplot(b,aes(x=newdeathsNum, y=newcasesNum,color=ffc))+geom_density_2d()+geom_point() + theme_bw()
print(plot+ mynamestheme + labs(x="New Deaths",y="New Cases",colour="Waves",title=str_c("New ceses vs new deaths\ncorrelation plot, delay=",as.character(delay),", ",country,sep="",collapse=NULL)) + geom_smooth(method=lm))

dev.off()

print(plot)
