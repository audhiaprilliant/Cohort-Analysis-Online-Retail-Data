# ===== PRE-PROCESSING - COHORT ANALYSIS REVENUE =====

# LOAD THE DATA
online.retail = read.csv2(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/DATASETS/Market Basket Analysis/Online Retail/Online Retail Data.csv',
                          header = TRUE,
                          sep = ';')
View(online.retail)
colnames(online.retail)
str(online.retail)

# PRE-PROCESSING - REMOVING DUPLICATES
cat('The number of non-duplicate observations within the data set is',
    nrow(unique(online.retail)),'out of','\n',
    nrow(online.retail),
    'indicating that there are',
    nrow(online.retail) - nrow(unique(online.retail)),
    'duplicates within the data set.','\n',
    'online.retail2 is our new, duplicate observation free data frame.')
index.duplicated = which(duplicated(online.retail)) # Index of the observations with duplicated values
online.retail2 = online.retail[-index.duplicated,] # Subsetting out the duplicated values using their index positions
rm(online.retail,index.duplicated) # Remove object Online Retail data from environment
View(online.retail2)

# PRE-PROCESSING - REMOVING NA's
cat('There are',
    nrow(online.retail2[!complete.cases(online.retail2),]),
    'out of',
    nrow(online.retail2),
    'total observations that feature at least one NA value.','\n',
    'The customerId column alone features all of the',
    nrow(online.retail2[!complete.cases(online.retail2$CustomerID),]),
    'of the total NA values. These','\n','observations will be exluded from the new data frame, online.retail3, gicing us a new total of',
    nrow(online.retail2) - nrow(online.retail2[!complete.cases(online.retail2),]),
    'observations to work with.')
online.retail3 = online.retail2[complete.cases(online.retail2),]
rm(online.retail2) # Remove object Online Retail 2 data from environment
View(online.retail3)

# PRE-PROCESSING - DATES FORMATTING
# Converting the InvoiceDate column into a date object
# online.retail3$InvoiceDate = substring(text = online.retail3$InvoiceDate,
#                                       first = 1,
#                                       last = 10)
# online.retail3$InvoiceDate = as.factor(online.retail3$InvoiceDate)
online.retail3$InvoiceDate = as.Date(online.retail3$InvoiceDate,
                                     format = '%d/%m/%Y')
# Setting up a column for the year data to make it easier to focus on 2011
online.retail3$Year = as.numeric(format(online.retail3$InvoiceDate,
                                        '%Y'))
# Calculating 'revenue' each transactions
table(sign(online.retail3$Quantity)) # There are negative values in Quantity variable
online.retail3$Quantity = abs(online.retail3$Quantity)
online.retail3$Revenue = online.retail3$Quantity * online.retail3$UnitPrice
# Taking data from 2011 only
cohort2011 = online.retail3[which(online.retail3$Year == 2011),]
# Dumping the unneeded variables
cohort2011 = cohort2011[,c('CustomerID',
                           'InvoiceDate',
                           'Year',
                           'Revenue')]
str(cohort2011)

# SAVE COHORT DATA INTO CSV FILE
write.csv(x = cohort2011,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Revenue/1 cohort2011.csv',
          row.names = FALSE)
