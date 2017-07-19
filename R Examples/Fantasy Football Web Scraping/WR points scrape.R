#*******************************
# Project: Fantasy Football
# Author: Jeff Lambert
# Date: 7/5/2017
# Program name: Fantasy Points Scraped from the Web.R
# Purpose: Crawl through selected weeks and season of footballdb.com, extract the raw data
#           and clean the data for the wide receivers.
#*******************************

#*******************************
# Change Log
# Program Creation - 7/5/2017
#
#*******************************

# Notes: Data retrieved from footballdb.com HTML.  Program is designed to retrieve data for the wide receiver
#         position.

#****************
# Load Packages
#****************


require(XML)
require(stringr)
require(plyr)
require(sqldf)


week <- 1:17 #Crawl through data from weeks 1:17 aka, the regular season
year <- c(2016) #Select the year of interest

p_url <- function(x){
  points_url <- paste0("http://www.footballdb.com/fantasy-football/index.html?pos=WR&yr="
                       ,x,"&wk=",week,"&ppr=")
  return(points_url)
} #This user defined function allows us to iterate through weeks of the selected season.

points_url <- sapply(2016,p_url) #Apply the defined function to the selected season.

points_url_parsed <- llply(points_url,htmlParse) #Parse the HTML from the URL vector "points_url".
points_data <- llply(points_url_parsed,readHTMLTable) #Read tables from the parsed HTML.

points_data_temp <- as.data.frame(points_data[[1]][1])[,c(1:3,14:17)] #Check point: Verify by visualization that the table
                                                                      # scraped from the URL is the correct table


p.name <-  function(x,set){
  name <- strsplit(as.character(set[,1]),",")[[x]][1]
  return(name)} #Function cleans player name column.

p.team <- function(x,set){
  team <- strsplit(as.character(set[,1]),",")[[x]][3]
  return(team)
} #Function used to create a variable for player team.


WR_v_name_clean <- function(vec){
  v_name_1 <- str_replace(vec[1:3],pattern="NULL.","")
  v_name_3 <- str_replace(vec[4:7],pattern="NULL.","")
  v_name_3 <- paste0('rec_',v_name_3)
  v_name <- c(v_name_1,v_name_3)
  v_name <- str_replace(v_name,pattern=".2+","")
  return(v_name)
}  #Function cleans the variable names from the raw datasets.

WR_point_cleaner <- function(x,set,yr){
  p <- as.data.frame(set[[x]][1])[,c(1:3,14:17)]
  p_1 <- p[,c(1,2)]
  p_2 <- p[,-c(1,2)]
  p_2 <- apply(p_2,2,as.numeric)
  p <- cbind(p_1,p_2)
  v_name <- WR_v_name_clean(vec=variable.names(p))
  p[,8] <- yr
  p[,9] <- x
  p[,10] <- ifelse(str_detect(p[,2],"@")==TRUE,"away","home")
  p[,2] <- str_extract(p[,2],pattern="[[:alpha:]]+")
  names(p) <- c(v_name,"year","week","home")
  return(p)
} #Function converts character strings to numeric where applicable, creates identifier variables for
# year and week, removes "@" symbole from opponent variable.

WR_points_cleaned <- ldply(1:17,WR_point_cleaner,set=points_data,yr=2016)
  #Run the point cleaner function through all selected weeks of the selected season. Stacks the 17
  # cleaned datasets into one dataset for 2016 wide receivers.
WR_points_cleaned$team <- sapply(1:nrow(WR_points_cleaned),p.team,set=WR_points_cleaned)
  #Create a variable for player team.
WR_points_cleaned[,1] <- sapply(1:nrow(WR_points_cleaned),p.name,set=WR_points_cleaned)
  #Clean up player name column.

Q <- c("CREATE TABLE WR2016 as SELECT * FROM WR_points_cleaned ") #SQL call for creating table WR2016 within the 
# SQL lite database

sqldf(Q, dbname="Fantasy Points Database.sqlite") 
#Call the SQL query on the "Fantasy Points Database.sqlite" database.

#Validation checkpoint: check that table has been added to DB, print the first 10 observations from table
db <- dbConnect(SQLite(),dbname="Fantasy Points Database.sqlite") 
dbListTables(db)
sqldf("SELECT * FROM WR2016 LIMIT 10",dbname="Fantasy Points Database.sqlite")


