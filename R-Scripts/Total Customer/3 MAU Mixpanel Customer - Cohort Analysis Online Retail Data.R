# ===== MAU MIXPANEL - COHORT ANALYSIS =====

# READ THE DATA
cohort2011 = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Customer/2 cohort2011 Cohort Formed - Total Customer.csv',
                      header = TRUE,
                      sep = ',')
str(cohort2011)
View(cohort2011)
# Structure Manipulation - Year and Month is not Date
cohort2011$InvoiceDate = as.Date(paste(cohort2011$InvoiceDate, 1, # We add day in InvoiceDate
                                       sep = '-'),
                                 format = '%Y-%m-%d')
cohort2011$InvoiceDate = format(cohort2011$InvoiceDate,
                                '%Y-%m')
cohort2011$Join_Date = as.Date(paste(cohort2011$Join_Date, 1, # We add day in Join_Date
                                     sep = '-'),
                               format = '%Y-%m-%d')
cohort2011$Join_Date = format(cohort2011$Join_Date,
                              '%Y-%m')
# Level for Cohort variable
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

# MAU MIXPANEL
# The day and month Age variables keep us from removing duplicates which is why we need to exclude them both
index.duplicated = which(duplicated(cohort2011[,c(-5,-6)]))
# Removing the duplicate observations
cohort2011 = cohort2011[-index.duplicated,]
rm(index.duplicated) # Remove object Index Duplicated vector from environment
# Creating rows for each cohort group
# Creating columns for each value in the Age_by_Month column;0-11
cohorts.wide = reshape2::dcast(data = cohort2011,
                               formula = Cohort ~ Age_by_Month,
                               value.var = 'CustomerID',
                               fun.aggregate = length)
View(cohorts.wide)

# SAVE COHORT WIDE DATA INTO CSV FILE
write.csv(x = cohorts.wide,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Customer/3 cohort2011 Wide Data.csv',
          row.names = FALSE)

# VISUALIZATION
# Creating 19 breaks and 20 rgb color values ranging from blue to white
breaks = quantile(cohorts.wide[,3:13],
                  probs = seq(.05, .95, .05),
                  na.rm = TRUE)
colors = sapply(round(seq(155, 80, length.out = length(breaks) + 1), 0),
                function(x){ rgb(x,x,155, maxColorValue = 155)})
# The Retention Mixpanel with counts
library(DT)
DT::datatable(cohorts.wide,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(ordering = FALSE,
                             dom = 't',
                             pageLength = 12)) %>%
  formatStyle("0",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatStyle(names(cohorts.wide[c(-1,-2)]),
              fontWeight = 'bold',
              color = 'white',
              backgroundColor = styleInterval(breaks,colors))
