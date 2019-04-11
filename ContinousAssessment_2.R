# Reading in the NI Post Code file.
# With this command you can select the file to read although if required you could hardcode the path/file name
# I forced all blank spaces to NA to make it easier to manage that as I move forward


NIPostCodeSource <- read.csv(file = "NIPostcodes.csv", header=FALSE, na.strings=c("","NA"))
str(NIPostCodeSource)
head(NIPostCodeSource, n =10L)


#setting the column names 
colnames(NIPostCodeSource) <- c("Org_name", "Sub_Building_name", "Building_No", "Number", "Primary_Thorfare", 
                               "Alt_Thorfare", "Secondary_Thorfare", "Locality", "Townland", "Town", 
                               "County", "Postocode", "x_coord",	"y_coord",	"PK")
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

crime_file_names <- list.files(full.names=TRUE)
crime_file_names

AllNICrimeData <- do.call(rbind,lapply(crime_file_names,read.csv))


setwd("..")
write.csv(AllNICrimeData, "AllNICrimeData.csv")

nrow(AllNICrimeData)
head(AllNICrimeData, n=2L)

str(AllNICrimeData)

AllNICrimeData = subset(AllNICrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name, Last.outcome.category, Context) )
head(AllNICrimeData, n=2L)


AllNICrimeData$Crime.type <- as.factor((AllNICrimeData$Crime.type))

AllNICrimeData$Location <- gsub('On or near ', '', AllNICrimeData$Location)

summary(AllNICrimeData)
AllNICrimeData$Location <- replace(AllNICrimeData$Location, AllNICrimeData$Location == '', NA)
head(AllNICrimeData, n=10L)


colSums(is.na(AllNICrimeData))
str(AllNICrimeData)

# select 1000 random records from AllNICrimeData where the Location is not NA - using the following command.

random_crime_sample <- AllNICrimeData[ sample( which(AllNICrimeData$Location !='NA'), 1000 ), ]
nrow(random_crime_sample)
head(random_crime_sample, 10)

getwd()

CleanNIPostCode <- read.csv(file = "CleanNIPostcodeData.csv", header=TRUE, na.strings=c("","NA"))

head(CleanNIPostCode, n=10L)

#  Matching Location in AllNICrime against the Primary_Thorfare in CleanNIPostCode file

match_result <- match(AllNICrimeData$Location, CleanNIPostCode$Primary_Thorfare)

match_result <- dplyr::left_join(AllNICrimeData, CleanNIPostCode, by= c("Location" = "Primary_Thorfare"), copy = FALSE)

head(match_result, 10L)

?dplyr::left_join


match_result

