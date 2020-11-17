# Scraping code for the Sars-Cov-2 data from worldometers.info
# Input: snapshots_new.dat - grid of dates and times of snapshots of worldometers.info
# Parameters: starting day (variable: start) (subselection from available snapshots) and ending day (variable: end)

# Loading the libraries
library(rvest)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Loading the list of snapshot times for the waybackmachine
list_snapshots <- read.delim2("snapshots_new.dat",header=FALSE,colClasses = "character")

# Function creating a list of urls from the list of snapshot times
date_filt<-function(snapshots,num_snapshots) {


snapshots_length<-dim(snapshots)[1]

out_vec <- vector(mode="character", length=num_snapshots)
count_j<-0
sub_s_a<-""

# Looping through the snapshots time
for(i in 1:snapshots_length) {

line_length<-14
# Information about the day
sub_s<-substr(snapshots[[1]][i],7,8)

# Picking only unique days
if(!(sub_s==sub_s_a)) {		# Comparing the new substring to the old one

count_j<-count_j+1

# Putting together the url
out_vec[count_j]<-str_c(snapshots[[1]][i],"/https://www.worldometers.info/coronavirus/",sep="",collapse=NULL)
out_vec[count_j]<-str_c("https://web.archive.org/web/",out_vec[count_j],sep="",collapse=NULL)

}

# Saving the old substring
sub_s_a<-sub_s

}

return(out_vec)
}

# Scraping function
# Input: urls, starting day, ending day
urls_tables<-function(urls,start,end) {

# Vector for storing the output
coronav <- vector(mode="list", length=end-start+1)

# Looping over the urls
for(i in start:end) {

# Printout for control
print(urls[i])

coronav[i-start+1] <- urls[i] %>%

# Scraping
  read_html() %>%
  html_nodes(xpath='//*[@id="main_table_countries_yesterday"]') %>%
  html_table()

print(i)
}

return(coronav)
}

# Setting starting day and ending day values

start<-20
end<-225

# Execution of Functions

urls<-date_filt(list_snapshots,end)

coronaV<-urls_tables(urls,start,end)
