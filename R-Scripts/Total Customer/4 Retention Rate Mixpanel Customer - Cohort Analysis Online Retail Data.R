# ===== RETENTION RATE MIXPANEL - COHORT ANALYSIS =====

# READ THE DATA
cw.retention = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Customer/3 cohort2011 Wide Data - Total Customer.csv',
                        header = TRUE,
                        sep = ',')
View(cw.retention)
# Structure Manipulation - Column names
colnames(cw.retention) = c('Cohort','0','1','2','3','4','5','6','7','8','9','10','11')

# RETENTION RATE MIXPANEL
# Calculating the percentages. Month number/join month number
for (i in rev(3:ncol(cw.retention))) {
  cw.retention[,i] = round(x = cw.retention[,i]/cw.retention[,2],
                           digits = 4)
}
rm(i) # Remove object i vector from environment
# Cloning the retention mixpanel
retention.avgs = cw.retention
# Converting 0.0000 to NAs
retention.avgs[retention.avgs == 0.0000] = NA
avgs.ret = round(apply(X = retention.avgs[,-1],
                       MARGIN = 2,
                       FUN = mean,
                       na.rm = TRUE),
                 digits = 4)
avgs.ret = c(0,avgs.ret) # Add zero for first column
# Adding the averages row to the retention mixpanel
cw.retention = rbind(cw.retention,avgs.ret)

# SAVE COHORT WIDE RETENTION DATA INTO CSV FILE
write.csv(x = cw.retention,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/CW Retention Rate.csv',
          row.names = FALSE)

# VISUALIZATION
# Creating 19 breaks and 20 rgb color values ranging from blue to white
breaks = quantile(cw.retention[,3:13],
                  probs = seq(.05, .95, .05),
                  na.rm = TRUE)
colors = sapply(round(seq(155, 80, length.out = length(breaks) + 1), 0),
                 function(x){ rgb(x,x,155, maxColorValue = 155)})
# The retention rate mixpanel
DT::datatable(cw.retention,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(ordering = FALSE,
                             dom = 't',
                             pageLength = 13)) %>%
  formatStyle("0",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatPercentage(c(3:13),2) %>% # We don't want column 0 in %
  formatStyle("1", 
              fontWeight = 'bold') %>%
  formatStyle(names(cw.retention[c(-1,-2)]),
              color = 'white',
              fontWeight = 'bold',
              backgroundColor = styleInterval(breaks,colors))