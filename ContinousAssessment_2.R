# Reading in the NI Post Code file into the NIPostCodeSource dataframe.
# I am assuming that the file "NIPostcodes.csv" is in the current working directory
# Please note I forced all blank spaces to "NA" to make it easier to manipulate as I move forward
# This also take care of the Step C action as rather than dropping those rows I replace with NA

NIPostCodeSource <- read.csv(file = "NIPostcodes.csv", header=FALSE, na.strings=c("","NA"))

# The following 3 commands provide the row count, the structure and display the first 10 rows of the dataframe
nrow(NIPostCodeSource)
str(NIPostCodeSource)
head(NIPostCodeSource, n =10L)


#setting the column names 
colnames(NIPostCodeSource) <- c("Org_name", "Sub_Building_name", "Building_No", "Number", "Primary_Thorfare", 
                               "Alt_Thorfare", "Secondary_Thorfare", "Locality", "Townland", "Town", 
                               "County", "Postcode", "x_coord",	"y_coord",	"PK")
head(NIPostCodeSource, 10)



# The following command provides the data structure of the NIPostCodeSource data frame
# and printing the first 10 rows of the data from using the head() function
structure(NIPostCodeSource, n = 10L)
head(NIPostCodeSource, n = 10L)

# The total number of missing values is shown with the summary command and it specifically lists the number of NA's
# Please note that for some data columns there are no NA's e.g. County, so NA's is not listed for that column.
summary(NIPostCodeSource)
mean(is.na(NIPostCodeSource))
# The request to return the mean missing values of the NIPOstcodedata doesn't make sense - need to discuss further.
# mean(NIPostCodeSource)

# For missing values I populated NA where no values were provided.   NA is the only logical value that could be provided.
# As County, x_coord and y_coord are all populated it may be possible to retreive key data like Town by joining with 
# some sort of mapping data but for this excercise leaving the values as NA and not removing them makes the most sense.


barplot(table(NIPostCodeSource$County))
table(NIPostCodeSource$County)

# Setting the County values as a categorizing factor using the as.factor command
NIPostCodeSource$County <- as.factor((NIPostCodeSource$County))

# moving the primary key to the start of the dataset using the following command
# First I do a head to show the order, then I reorder by moving the 15th attribute 
# PK, to the first column followed by the next 14

NIPostCodeSource<-NIPostCodeSource[,c(15, 1:14)]


head(NIPostCodeSource, n = 2L)
str(NIPostCodeSource)

# To filter out all records that have the text LIMAVADY in either Town, Townland or Locality I had to use the dplyr::filter package
# It wasn't clear on reading the requirements whether the condition was an AND or an OR - so I selected records if LIMAVADY was 
# populated in any of the 3 fields.
# In order to use the dplyr function I installed the package and then called the library and ran the filter to populate Limavady_Data

install.packages("dplyr")
library(dplyr)
Limavady_Data <- dplyr::filter(NIPostCodeSource, grepl('LIMAVADY', Town) | grepl('LIMAVADY', Townland) | grepl('LIMAVADY', Locality))
nrow(Limavady_Data)

# After creating Limavady data I run these commands to verify the population and then write the data out to Limavady.csv file
head(Limavady_Data, n = 2L)
str(Limavady_Data)
write.csv(Limavady_Data, "Limavady.csv")


str(NIPostCodeSource)

# Write out the NIPostCodeSource dataframe to CleanNIPostcodeData.csv on the current directory

write.csv(NIPostCodeSource, "CleanNIPostcodeData.csv")


#  In this step I navigate to the sub-directory NI Crime Data where all the csv files should be stored.
getwd()
setwd("NI Crime Data")
getwd()

# In the next steps read all of the files listed in the sub-directory NI Crime Data into a file called "crime_file_names".   
# This file is then fed into the rbind command which reads all of the seperte files into the AllNICrimeData variable.
# The key to these 2 steps is to ensure that all files that you wish to read are in the same directory - 
# which I did by manually putting them into one directory

crime_file_names <- list.files(full.names=TRUE)
AllNICrimeData <- do.call(rbind,lapply(crime_file_names,read.csv))
nrow(AllNICrimeData)

head(AllNICrimeData, 5)
str(AllNICrimeData)

# Moving back up the directory and writing out the AllNICrimeData.csv file which combined all of the different Crime Data files.

setwd("..")
write.csv(AllNICrimeData, "AllNICrimeData.csv")


str(AllNICrimeData)

# Removing columns from AllNICrimeData that we do not want - by using the -c command and subset as below.
AllNICrimeData = subset(AllNICrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name, Last.outcome.category, Context) )
head(AllNICrimeData, n=10)

# Setting Crime Type as a factor
AllNICrimeData$Crime.type <- as.factor((AllNICrimeData$Crime.type))

# Removing the text 'On or near ' from the Location value in AllNICrimeData
AllNICrimeData$Location <- gsub('On or near ', '', AllNICrimeData$Location)

# For the Location field, populating 'NA' for all fields that are blank. 
# This will allow us to the filter/identify records where the location is blank.

AllNICrimeData$Location <- replace(AllNICrimeData$Location, AllNICrimeData$Location == '', NA)
head(AllNICrimeData, n=10L)


colSums(is.na(AllNICrimeData))
str(AllNICrimeData)

# select 1000 random records from AllNICrimeData where the Location is not NA - using the following command.
# I also used sapply to turn all the text to upper class - to allow for a cleaner comparison between the NI Postcode file

random_crime_sample <- AllNICrimeData[ sample( which(AllNICrimeData$Location !='NA'), 1000 ), ]
random_crime_sample = as.data.frame(sapply(random_crime_sample, toupper))
nrow(random_crime_sample)
head(random_crime_sample, 10)

#######################################################

#dplyr::filter(most_frequent_PostCode, grepl('BARRS LANE', Primary_Thorfare) )
#dplyr::filter(new_CleanNIPostCode, grepl('BARRS LANE', Primary_Thorfare) )

#################################################



# Function to find a post code based on the location in the crime_data file.
# Firstly I read in the CleanNIPostcodeData.csv file and then I remove all rows to leave only the Primary_Thorfare and Postcode
# I then populated most_frequent_postCode with the most frequent postcode found for the same Primary_Thorfare - 
# because you can have multiple different throughfare values
# These 3 commands leave me with a list of street locations and their corresponding postcodes taking the most populate postcode 
# which will allow me to compare and populate the appropriate postcode by matching with the crime file

find_a_postcode <- function(crime_data){
  library(dplyr)
  
  new_CleanNIPostCode <- read.csv(file = "CleanNIPostcodeData.csv", header=TRUE, na.strings=c("","NA"))
  new_CleanNIPostCode = subset(new_CleanNIPostCode, select = -c(PK, Org_name, Sub_Building_name, Building_No, Number, Alt_Thorfare, Secondary_Thorfare, Locality, Townland, Town, County, x_coord, y_coord ) )
  most_frequent_PostCode <- new_CleanNIPostCode %>% group_by(Primary_Thorfare) %>% summarize(Postcode =names(which.max(table(Postcode))))
  
  # In this next step I do a left join on the crime file against the most frequnet postcode by joining the Location and Primary_Thorfare.
  # This appends the Postcode to the crime file based on the street address.
  # In this case I put the results into match_result and the removed any records where the Postcode are NA which indicates that a match was not found
  
  match_result <- dplyr::left_join(crime_data, most_frequent_PostCode, by=c("Location" = "Primary_Thorfare"))
  match_result <- match_result[!is.na(match_result$Postcode), ]
  
  return(match_result)
}

# Calling the find_a_postcode function and passing the crime sample dataframe
# Populating the results into the crime_data_with_postcode dataframe.
crime_data_with_postcode <- find_a_postcode(random_crime_sample)

str(random_crime_sample)
nrow(random_crime_sample)
head(random_crime_sample, 10)
tail(random_crime_sample, 10)

# command to combine both dataframes togehter - with the new Postcode field being added and populated with nulls.

library(plyr)
random_crime_sample <- rbind.fill(random_crime_sample, crime_data_with_postcode)
write.csv(random_crime_sample, "random_crime_sample.csv")

# Command to extract all those records from random_crime_sample that have BT1 in the Postcode
# and sort by Postcode and then Crime.type

chart_data <- dplyr::filter(random_crime_sample, grepl('BT1', Postcode) )
chart_data <- chart_data %>% arrange(Postcode, Crime.type)

summary(chart_data$Crime.type)


head(chart_data, 10)
str(chart_data)
tail(chart_data, 20)

nrow(chart_data)



#increase margin
my_vector=c(3,12,5,18,45)
names(my_vector)=c("A","B","C","D","E")
par(mar=c(11,4,4,4))
barplot(my_vector, col=rgb(0.2,0.4,0.6,0.6), names.arg=c("very long group name 1",
                                                         "very long group name 2",
                                                         "very long group name 3",
                                                         "very long group name 4",
                                                         "very long group name 5"), las=2 )
chart_table <- table(chart_data$Crime.type)
chart_table[order(Crime.Type)]
chart_table

main <- "par(cex.axis=2, cex.lab=2, cex.main=1.2, cex.sub=1)"

barplot(table(chart_data$Crime.type), 
        ylab='Number of occurences', 
        xlab='Crime Type',
#        las=2,
        main=main)




