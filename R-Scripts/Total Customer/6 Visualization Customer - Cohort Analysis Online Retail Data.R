# ===== VISUALIZATION - COHORT ANALYSIS =====

library(reshape2)
library(ggplot2)

# VISUALIZATION OF TOTAL CUSTOMER
# Load the Data
cohorts.wide = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Customer/3 cohort2011 Wide Data - Total Customer.csv',
                        header = TRUE,
                        sep = ',')
View(cohorts.wide)
# Structure Manipulation - Column names
colnames(cohorts.wide) = c('Cohort','M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11')
# Cohort Manipulation Name
caps = c('Jan Cohorts' = 'Cohort01 Jan',
         'Feb Cohorts' = 'Cohort02 Feb',
         'Mar Cohorts' = 'Cohort03 Mar',
         'Apr Cohorts' = 'Cohort04 Apr',
         'May Cohorts' = 'Cohort05 May',
         'Jun Cohorts' = 'Cohort06 Jun',
         'Jul Cohorts' = 'Cohort07 Jul',
         'Aug Cohorts' = 'Cohort08 Aug',
         'Sep Cohorts' = 'Cohort09 Sep',
         'Oct Cohorts' = 'Cohort10 Oct',
         'Nov Cohorts' = 'Cohort11 Nov',
         'Dec Cohorts' = 'Cohort12 Dec')
cohorts.wide$Cohort = as.character(cohorts.wide$Cohort)
cohorts.wide$Cohort = caps[cohorts.wide$Cohort]
# Preparing to put in function
cohort.customer.r = cohorts.wide # Create new data frame
totcols = ncol(cohort.customer.r) # Count number of columns in data set
# Function for Bottom Triangle Matrix
for (i in 1:nrow(cohort.customer.r)) { # For loop for shifting each row
  df = cohort.customer.r[i,] # Select row from data frame
  df = df[,!df[]==0] # Remove columns with zeros
  partcols = ncol(df) # Count number of columns in row (w/o zeros)
  
  if (partcols <= totcols) {
    df.not.null = as.vector(df[,2:partcols])
    df.null = t(rep(x = 0,
                    length.out = totcols - partcols))
    df = cbind(df.null,df.not.null) # Fill columns after values by zeros
  }
  cohort.customer.r[i,c(2:totcols)] = df # Replace initial row by new one
} # Used in this case
# FUNCTION FOR UP TRIANGLE MATRIX
for (i in 1:nrow(cohort.customer.r)) { # For loop for shifting each row
  df = cohort.customer.r[i,] # Select row from data frame
  df = df[,!df[] == 0] # Remove columns with zeros
  partcols = ncol(df) # Count number of columns in row (w/o zeros)
  
  if (partcols < totcols) {
    df[, c((partcols+1):totcols)] = 0 # Fill columns after values by zeros
  }
  cohort.customer.r[i,] = df # Replace initial row by new one
} # Not used in this case
View(cohort.customer.r)
# Melting Data
cohort.chart.customer = reshape2::melt(data = cohort.customer.r,
                                       id.vars = 'Cohort')
colnames(cohort.chart.customer) = c('Cohort','Month','Customer')

# Define palette
reds = colorRampPalette(c('pink','dark red'))
# Plot data
ggplot(data = cohort.chart.customer)+
  geom_area(aes(x = Month,
                y = Customer,
                group = Cohort,
                fill = Cohort))+
  scale_fill_manual(values = reds(nrow(cohort.customer.r)))+
  labs(title = 'Active Customer by Cohort',
       subtitle = 'Online Retail Data',
       caption = 'Customer in 2011')+
  theme_bw()

# VISUALIZATION OF RETENTION RATE CUSTOMER
# Load the Data
cw.retention = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Customer/4 CW Retention Rate - Total Customer.csv',
                        header = TRUE,
                        sep = ',')
View(cw.retention)
# Structure Manipulation - Column names
colnames(cw.retention) = c('Cohort','M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11')
cw.retention = cw.retention[1:12,-2] # Remove M0 data because it is always 100%
# Cohort Manipulation Name
caps = c('Jan Cohorts' = 'Cohort01 Jan',
         'Feb Cohorts' = 'Cohort02 Feb',
         'Mar Cohorts' = 'Cohort03 Mar',
         'Apr Cohorts' = 'Cohort04 Apr',
         'May Cohorts' = 'Cohort05 May',
         'Jun Cohorts' = 'Cohort06 Jun',
         'Jul Cohorts' = 'Cohort07 Jul',
         'Aug Cohorts' = 'Cohort08 Aug',
         'Sep Cohorts' = 'Cohort09 Sep',
         'Oct Cohorts' = 'Cohort10 Oct',
         'Nov Cohorts' = 'Cohort11 Nov',
         'Dec Cohorts' = 'Cohort12 Dec')
cw.retention$Cohort = as.character(cw.retention$Cohort)
cw.retention$Cohort = caps[cw.retention$Cohort]
# Melting Data
cohort.chart.ret.rate = reshape2::melt(data = cw.retention,
                                       id.vars = 'Cohort')
colnames(cohort.chart.ret.rate) = c('Cohort','Month','Retention')

cohort.chart.ret.rate = cohort.chart.ret.rate[which(cohort.chart.ret.rate$Retention != 0),]
# Dynamics Analysis Chart
ggplot(data = cohort.chart.ret.rate,
       aes(x = Month,
           y = Retention,
           group = Cohort,
           colour = Cohort))+
  geom_line(size = 2,
            alpha = 0.5)+
  geom_point(size = 3,
             alpha = 1)+
  geom_smooth(aes(group = 1),
              method = 'loess',
              size = 2,
              colour = 'black',
              se = FALSE)+
  labs(title = 'Cohorts Retention Rate Dynamics',
       subtitle = 'Online Retail Data',
       caption = 'Customer in 2011')+
  theme_bw()

# VISUALIZATION OF AVERAGE REVENUE PER CUSTOMER BY COHORT
# Load the Data - Total Customer
total.customer = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Customer/3 cohort2011 Wide Data - Total Customer.csv',
                          header = TRUE,
                          sep = ',')
# Load the Data - Total Revenue
total.revenue = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Revenue/3 cohort2011 Wide Data - Total Revenue.csv',
                         header = TRUE,
                         sep = ',')
# Structure Manipulation - Column names
colnames(total.customer) = c('Cohort','M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11')
colnames(total.revenue) = c('Cohort','M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11')
# Cohort Manipulation Name
caps = c('Jan Cohorts' = 'Cohort01 Jan',
         'Feb Cohorts' = 'Cohort02 Feb',
         'Mar Cohorts' = 'Cohort03 Mar',
         'Apr Cohorts' = 'Cohort04 Apr',
         'May Cohorts' = 'Cohort05 May',
         'Jun Cohorts' = 'Cohort06 Jun',
         'Jul Cohorts' = 'Cohort07 Jul',
         'Aug Cohorts' = 'Cohort08 Aug',
         'Sep Cohorts' = 'Cohort09 Sep',
         'Oct Cohorts' = 'Cohort10 Oct',
         'Nov Cohorts' = 'Cohort11 Nov',
         'Dec Cohorts' = 'Cohort12 Dec')
total.customer$Cohort = as.character(total.customer$Cohort)
total.revenue$Cohort = as.character(total.revenue$Cohort)
total.customer$Cohort = caps[total.customer$Cohort]
total.revenue$Cohort = caps[total.revenue$Cohort]
# Making Data Frame for Average Revenue
rev.per.client = total.revenue[,c(2:13)]/total.customer[,c(2:13)]
rev.per.client[is.na(rev.per.client)] = 0
rev.per.client = cbind(Cohort = total.customer[,1],
                       rev.per.client)
# Preparing to put in function
totcols = ncol(rev.per.client) # Count number of columns in data set
# Function for Bottom Triangle Matrix
for (i in 1:nrow(rev.per.client)) { # For loop for shifting each row
  df = rev.per.client[i,] # Select row from data frame
  df = df[,!df[]==0] # Remove columns with zeros
  partcols = ncol(df) # Count number of columns in row (w/o zeros)
  
  if (partcols <= totcols) {
    df.not.null = as.vector(df[,2:partcols])
    df.null = t(rep(x = 0,
                    length.out = totcols - partcols))
    df = cbind(df.null,df.not.null) # Fill columns after values by zeros
  }
  rev.per.client[i,c(2:totcols)] = df # Replace initial row by new one
} # Used in this case

# Melting Data
rev.per.client.chart = reshape2::melt(data = rev.per.client,
                                      id.vars = 'Cohort')
colnames(rev.per.client.chart) = c('Cohort','Month','Average_Revenue')
# Define palette
greens = colorRampPalette(c('light green','dark green'))
# Plot data
ggplot(data = rev.per.client.chart)+
geom_area(aes(x = Month,
              y = Average_Revenue,
              group = Cohort,
              fill = Cohort))+
  scale_fill_manual(values = greens(nrow(rev.per.client)))+
  ylab('Average Revenue')+
  labs(title = 'Average Revenue per Customer by Cohort',
       subtitle = 'Online Retail Data',
       caption = 'Customer in 2011')+
  theme_bw()
