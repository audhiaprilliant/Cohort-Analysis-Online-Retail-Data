# ===== CREATING COHORT - COHORT ANALYSIS =====

# READ THE DATA
cohort2011 = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Customer/1 cohort2011 - Total Customer.csv',
                      header = TRUE,
                      sep = ',')
str(cohort2011)
View(cohort2011)
cohort2011$InvoiceDate = as.Date(cohort2011$InvoiceDate,
                                 format = '%Y-%m-%d')

# CREATING THE COHORT
# Getting the first transaction dates for each customer
join.date = aggregate(InvoiceDate ~ CustomerID,
                      data = cohort2011,
                      FUN = min,
                      na.rm = TRUE)
# Changing the name of the column InvoiceDate to Join_Date
colnames(join.date)[2] = 'Join_Date'
# Merge the join.date data to the cohort2011 data frame
cohort2011 = merge(x = cohort2011,
                   y = join.date,
                   by.x = 'CustomerID',
                   by.y = 'CustomerID',
                   all.x = TRUE)
# Creating the groups or cohort based on the join date month
cohort2011$Cohort = as.numeric(format(cohort2011$Join_Date,
                                      '%m'))
rm(join.date) # Remove object Join Date data from environment

# COHORT AGE
# Calculating the difference in days between the invoice date column by join date column
cohort2011$Age_by_Day = as.numeric(difftime(time1 = cohort2011$InvoiceDate,
                                            time2 = cohort2011$Join_Date,
                                            units = 'days'))
# Dividing the days by 30 to get the number of months
cohort2011$Age_by_Month = floor(cohort2011$Age_by_Day/30)
# Dumping the day element from the join date column
cohort2011$Join_Date = format(cohort2011$Join_Date,
                              '%Y-%m')
# Remove the day element from the InvoiceDate data since this Cohort Analysis is based on monthly activity
cohort2011$InvoiceDate = format(cohort2011$InvoiceDate,
                                '%Y-%m')
# Relabel the cohort column data to something more intuitive for the sake of the report consumers, then factor them since these are sequential
groups = c('Jan Cohorts',
           'Feb Cohorts',
           'Mar Cohorts',
           'Apr Cohorts',
           'May Cohorts',
           'Jun Cohorts',
           'Jul Cohorts',
           'Aug Cohorts',
           'Sep Cohorts',
           'Oct Cohorts',
           'Nov Cohorts',
           'Dec Cohorts')
for(i in 1:length(groups)){
  cohort2011[cohort2011$Cohort == i,'Cohort'] = groups[i]
}

cohort2011$Cohort = factor(cohort2011$Cohort,
                           ordered = TRUE,
                           levels = c('Jan Cohorts',
                                      'Feb Cohorts',
                                      'Mar Cohorts',
                                      'Apr Cohorts',
                                      'May Cohorts',
                                      'Jun Cohorts',
                                      'Jul Cohorts',
                                      'Aug Cohorts',
                                      'Sep Cohorts',
                                      'Oct Cohorts',
                                      'Nov Cohorts',
                                      'Dec Cohorts'))

# SAVE COHORT DATA INTO CSV FILE
write.csv(x = cohort2011,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/cohort2011 Cohort Formed.csv',
          row.names = FALSE)