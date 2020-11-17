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

# Filtering the row with the selected country and storing it
c_no_d<-coronaV[[i+start-1]] %>% filter_all(any_vars(. %in% country))

c_length<-dim(c_no_d)[1]

# Storing 'NewDeaths' quantity in newdeaths vector
new_deaths[i-delay]<-c_no_d[c_length,'NewDeaths']

tot_dead<-as.numeric(gsub(",","",c_no_d[c_length,'TotalDeaths']))
tot_reco<-as.numeric(gsub(",","",c_no_d[c_length,'TotalRecovered']))

# Storing the death rate quantity in deathrate vector
death_rate[i-delay]<-(tot_dead/(tot_dead+tot_reco))

}

# Looping over the days, new cases
# Starting at start
for(i in 1:(end-start+1)) {

# Filtering the row with the selected country and storing it
c_d<-coronaV[[i+start-1]] %>% filter_all(any_vars(. %in% country))

c_length<-dim(c_d)[1]

new_cases[i]<-c_d[c_length,'NewCases']

}

# Vectors to store various averaged variables
# Length depends on the parameters delay, start and end
new_cases_a <- vector(mode="character", length=(end-start+1-av+1))
new_deaths_a <- vector(mode="character", length=(end-start+1-av+1))
death_rate_a <- vector(mode="character", length=(end-start+1-av+1))

# Lower f_s and upper f_e bound for averaging
f_s<-floor(av/2.)
f_e<-ceiling(av/2.)-1

# Looping over vector elements and averaging
for(i in (f_s+1):(end-start+1-f_e)) {

new_cases_a[i-f_s] <- mean(as.numeric(gsub(",","",new_cases[(i-f_s):(i+f_e)])),na.rm=TRUE)

new_deaths_a[i-f_s] <- mean(as.numeric(gsub(",","",new_deaths[(i-f_s):(i+f_e)])),na.rm=TRUE)

death_rate_a[i-f_s] <- mean(as.numeric(gsub(",","",death_rate[(i-f_s):(i+f_e)])),na.rm=TRUE)
}

# Auxiliary vector
days<-start:(end-av+1)

# Numeric vector
new_cases_num<-as.numeric(gsub(",","",new_cases_a))

# Numeric vector
new_deaths_num<-as.numeric(gsub(",","",new_deaths_a))

# Labeling of days
days_f<-ifelse(days>12,ifelse(days>=80,"2nd","1st"),"Outliers")

# Data frame to plot
cor_frame_fin <- data.frame(days_f,as.numeric(new_deaths_num),as.numeric(new_cases_num))

# Aesthetic settings for plotting
my_names_theme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5), 
                 legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"), 
                 legend.text = element_text(face = "italic", colour="black",family = "Helvetica"), 
                  axis.title = element_text(family = "Helvetica", size = (10), colour = "black"),
                  axis.text = element_text(family = "Helvetica", colour = "black", size = (10)))

# Opening a file to print the plot in
png(str_c("av",av,"-",country,"noav-del",delay,"-full.png",sep="",collapse=NULL))

# The plot
plot<-ggplot(cor_frame_fin,aes(x=new_deaths_num, y=new_cases_num,color=days_f))+geom_density_2d()+geom_point() + theme_bw()

dev.off()

# Print the plot
print(plot+ my_names_theme + labs(x="New Deaths",y="New Cases",colour="Waves",title=str_c("New ceses vs new deaths\ncorrelation plot, delay=",as.character(delay),", ",country,sep="",collapse=NULL)) + geom_smooth(method=lm))

