
#loading packages to compile datasets
library(dplyr)
library(tidyr)

#importing data files (change file directories as needed)

#upload party noise complaint dataset. Data comes from https://www.kaggle.com/somesnm/partynyc
party <- read.csv('C://Users/kaylenejoy/Dropbox/Other Stuff/Data Science/R/R Ladies Meetup Dec 17/party_in_nyc.csv')


#upload dataset matching zip codes to PUMAs (Public Use Microdata Areas; geo units of 100k ppl)
#data comes from https://www.baruch.cuny.edu/confluence/display/geoportal/NYC+Geographies
zip_to_puma <- read.csv('C://Users/kaylenejoy/Dropbox/Other Stuff/Data Science/R/R Ladies Meetup Dec 17/nyc_zip_to_puma.csv')


#upload dataset with demographic information per PUMA
#data comes from https://www.baruch.cuny.edu/confluence/display/geoportal/NYC+Geographies
# but I did some compiling/selecting in excel first to get it to this point (tsk tsk)
puma_demo <- read.csv('C://Users/kaylenejoy/Dropbox/Other Stuff/Data Science/R/R Ladies Meetup Dec 17/select_puma_demo_info.csv')





#_________________CLEANING UP EACH DATASET______________________

###party database is ready to go###



###zip_to_puma###
#removing unneeded variables
zip_to_puma <- zip_to_puma %>%
  transmute(zip = zcta10, puma = puma10)



####puma demo###
#the puma variable (called Subject) has a bunch of extra text in it
#we want to extract the 4-digit number and remove the rest of the text/numbers
puma_demo$Subject <- sub('.*(\\d{4}).*', '\\1', puma_demo$Subject) #replace everything but the 4 digit number with nothing

puma_demo <- puma_demo %>%
  rename(puma = Subject) #just renaming this so it's clear this is the puma #





#_________________JOINING THE PARTY DATA TO THE DEMOGRAPHIC DATA___________________

#first, I'm adding the pumas to the party dataset
#check the classes of the each of the key variables
class(party_puma$Incident.Zip)
class(zip_to_puma$zip) #Both integer; that'll work

#joining pumas to the party dataset
party_puma <- party %>%
  left_join(zip_to_puma, by = c('Incident.Zip' = 'zip')) #we want to preserve all the data in party; only need rows from zip_to_puma that are in party; hence, left join


#checkING for places where the join didn't work
party %>%
  anti_join(zip_to_puma, by = c('Incident.Zip' = 'zip')) %>% #There are 2894 NAs here.  Is this due to missing zip code info?
  filter(!is.na(Incident.Zip)) %>% #There are 1904 NAs now. Let's group by zip so we can figure out which ones went wrong
  select(Incident.Zip) %>%
  group_by(Incident.Zip) %>%
  summarise(n())   #which zips codes didn't join? 
  
#most of the unmatched lines (1755 of them) come from 11249, a zipcode added to Brooklyn in 2011 (after demo);
#This area was formerly zip code 11211, making this zip code part of 
#other unmatched zipcodes include data entry errors (no zip code 83 or 10000) or zip codes at the edge of nyc;
#I'm not going to worry about these because they only impact ~150 cases 
party_puma$puma[party_puma$Incident.Zip == 11249] <- 4001
summary(party_puma$puma)
party_puma %>%
  filter(is.na(puma)) %>% #There are 1904 NAs now. Let's group by zip so we can figure out which ones went wrong
  summarise(n()) #there are now 1139 cases missing 
party_puma %>%
  filter(is.na(Incident.Zip)) %>%
  summarise(n()) #990 of these are bc they are missing zip codes in the party data; that's good enough for now
class(party_puma$puma)
class(puma_demo$puma)
####joining party_puma data with the puma_demo data

#checking the classes before joining
class(party_puma$puma)
class(puma_demo$puma) #ooop, need to change to a numeric
puma_demo$puma <- as.numeric(as.character(puma_demo$puma))

party_ready <- party_puma %>%
  left_join(puma_demo, by = 'puma') %>% #left join again because we only primarily about preserving the party pumas
  rename(created_date = Created.Date, closed_date = Closed.Date, location_type = Location.Type, incident_zip = Incident.Zip) #I dislike variables names with periods

#checking to see if there were any issue with the join
party_puma %>%
  anti_join(puma_demo, by = 'puma') %>%
  summarise(n())
#1139 didn't join; that's how many in the party dataset didn't hav`e a puma, so we're good

#just changing a few variable names so they are all lower case
names(party_ready) <- tolower(names(party_ready))

#and I want to change the date/time variables to be POSIXct so they are treated as dates
party_ready$created_date <- as.POSIXct(as.character(party_ready$created_date),format="%m/%d/%Y %H:%M")
party_ready$closed_date <- as.POSIXct(as.character(party_ready$closed_date),format="%m/%d/%Y %H:%M")
#now we've got our party_ready data!




#_________example how to restructure the data if you'd like to predict from demo characteristics

#example of how to restructure the data if you'd like to look at number of incidents 


#I want to create a variable for how many incidents are in each puma and
#I also want to create a variable for which location type is most common in each puma
#and then one about the average time to complete an incident

#This is a function to determine the mode, because that is not built into R
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#making the new dataset with modal location and number of incidents per puma.
#there will be 55 rows of this database, one for each puma
party_sideways <- party_ready %>%
  mutate(time_complete = closed_date-created_date) %>%
  group_by(puma) %>%
  mutate(num_of_incidents = n(), modal_location = getmode(location_type), avg_time_complete = mean(time_complete, na.rm = T)) %>%
  ungroup() %>%
  arrange(puma) %>%
  filter(!duplicated(puma))
  

write.csv(party_ready,'C://Users/kaylenejoy/Dropbox/Other Stuff/Data Science/R/R Ladies Meetup Dec 17/party_ready.csv')
write.csv(party_sideways,'C://Users/kaylenejoy/Dropbox/Other Stuff/Data Science/R/R Ladies Meetup Dec 17/party_sideways.csv')

