# Reading in the NI Post Code file.
# With this command you can select the file to read although if required you could hardcode the path/file name
# I forced all blank spaces to NA to make it easier to manage that as I move forward


NIPostCodeSource <- read.csv(file = "NIPostcodes.csv", header=FALSE, na.strings=c("","NA"))
str(NIPostCodeSource)
head(NIPostCodeSource, n =10L)


#setting the column names 
colnames(NIPostCodeSource) <- c("Org_name", "Sub_Building_name", "Building_No", "Number", "Primary_Thorfare", 
                               "Alt_Thorfare", "Secondary_Thorfare", "Locality", "Townland", "Town", 
                               "County", "Postcode", "x_coord",	"y_coord",	"PK")
head(NIPostCodeSource)


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
str(NIPostCodeSource)

# moving the primary key to the start of the dataset using the following command
# First I do a head to show the order, then I reorder by moving the 15th attribute, PK, to the first column followed by the next 14

head(NIPostCodeSource, n = 2L)
NIPostCodeSource<-NIPostCodeSource[,c(15, 1:14)]
head(NIPostCodeSource, n = 2L)

# To filter out all records that have the text LIMAVADY in either Town, Townland or Locality I had to use the dplyr::filter package
# First I installed the package and then called the library and ran the filter to populate Limavady_Data

install.packages("dplyr")
library(dplyr)
Limavady_Data <- dplyr::filter(NIPostCodeSource, grepl('LIMAVADY', Town) | grepl('LIMAVADY', Townland) | grepl('LIMAVADY', Locality))
Limavady_Data
nrow(NIPostCodeSource)
nrow(Limavady_Data)

# After creating Limavady data I run these commands to verify the population and then write the data out to Limavady.csv file
head(Limavady_Data, n = 2L)
str(Limavady_Data)

write.csv(Limavady_Data, "Limavady.csv")


write.csv(NIPostCodeSource, "CleanNIPostcodeData.csv")

str(NIPostCodeSource)

# Reading in the Crime Data from the zip file

#crime_data <- read.table("NI Crime Data.zip", nrows=10, header=T, quote="\"", sep=",")




getwd()
setwd("NI Crime Data")
getwd()

# In the next steps read all of the files listed in the sub-directory NI Crime Data into a file called crime_file_names.   
# This file is then fed into the rbind command which reads all of the seperte files into the AllNICrimeData variable.
# The key to these 2 steps is to ensure that all files that you wish to read are in the same directory - which I did by manually putting them into one directory

crime_file_names <- list.files(full.names=TRUE)
AllNICrimeData <- do.call(rbind,lapply(crime_file_names,read.csv))


# Moving back up the directory and writing out the AllNICrimeData.csv file which combined all of the different Crime Data files.

setwd("..")
write.csv(AllNICrimeData, "AllNICrimeData.csv")

nrow(AllNICrimeData)
head(AllNICrimeData, n=2L)

str(AllNICrimeData)

# Removing columns from AllNICrimeData that we do not want - by using the -c command and subset as below.
AllNICrimeData = subset(AllNICrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name, Last.outcome.category, Context) )
head(AllNICrimeData, n=2L)

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
nrow(random_crime_sample)
head(random_crime_sample, 10)
random_crime_sample = as.data.frame(sapply(random_crime_sample, toupper))

test_random_crime_sample <- AllNICrimeData[ sample( which(AllNICrimeData$Location !='NA'), 10 ), ]
head(test_random_crime_sample, 10)
test_random_crime_sample = as.data.frame(sapply(test_random_crime_sample, toupper))

#######################################################

dplyr::filter(most_frequent_PostCode, grepl('BARRS LANE', Primary_Thorfare) )
dplyr::filter(new_CleanNIPostCode, grepl('BARRS LANE', Primary_Thorfare) )

head(test_random_crime_sample, 2)
head(most_frequent_PostCode, 2)

head(new_CleanNIPostCode, 2)
new_CleanNIPostCode <- read.csv(file = "CleanNIPostcodeData.csv", header=TRUE, na.strings=c("","NA"))
new_CleanNIPostCode = subset(new_CleanNIPostCode, select = -c(PK, Org_name, Sub_Building_name, Building_No, Number, Alt_Thorfare, Secondary_Thorfare, Locality, Townland, Town, County, x_coord, y_coord ) )
most_frequent_PostCode <- new_CleanNIPostCode %>% group_by(Primary_Thorfare) %>% summarize(Postcode = names(which.max(table(Postcode))))

new_CleanNIPostCode %>% group_by(Primary_Thorfare) %>% summarize (Postcode =names(which.max(table(Postcode)))) 

match_result <- dplyr::left_join(test_random_crime_sample, most_frequent_PostCode, by=c("Location" = "Primary_Thorfare"))

head(match_result, 10)

match_result <- match_result[!is.na(match_result$Postcode), ]

nrow(match_result)

#test_CleanNIPostCode <- dplyr::filter(CleanNIPostCode, grepl('SEYMOUR', Primary_Thorfare) )

#################################################
head(test_CleanNIPostCode)
getwd()

test_match <- merge(test_random_crime_sample,test_CleanNIPostCode, by.x="Location", by.y="Primary_Thorfare", sort=F)

head(test_match)

head(CleanNIPostCode, n=10L)

# Function to fine a post code based on the location in the crime_data file.
find_a_postcode <- function(crime_data){
  
  # Firstly I read in the CleanNIPostcodeData.csv file and then I remove all rows to leave only the Primary_Thorfare and Postcode
  # I then populated most_frequent_postCode with the most frequent postcode found for the same Primary_Thorfare - because you can have multiple different throughfare values
  # These 3 commands leave me with a list of street locations and their corresponding postcodes - which will allow me to compare and populate the appropriate postcode by matching with the crime file
  
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

head(test_random_crime_sample)

crime_data_with_postcode <- find_a_postcode(random_crime_sample)

head(crime_data_with_postcode, 10)
nrow(crime_data_with_postcode)

