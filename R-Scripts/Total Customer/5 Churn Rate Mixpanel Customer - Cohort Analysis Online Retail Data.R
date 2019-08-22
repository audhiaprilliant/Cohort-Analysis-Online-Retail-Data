# ===== CHURN RATE MIXPANEL - COHORT ANALYSIS =====

# READ THE DATA
cw.churn = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/cohort2011 Wide Data.csv',
                    header = TRUE,
                    sep = ',')
View(cw.churn)
# Structure Manipulation - Column names
colnames(cw.churn) = c('Cohort','0','1','2','3','4','5','6','7','8','9','10','11')

# CHURN RATE MIXPANEL
for (i in rev(3:ncol(cw.churn))){
  #Calculates the retention rate
  cw.churn[,i] = round(x = cw.churn[,i]/cw.churn[,2],
                       digits = 4)
  
  # Turns the retention rate into the churn rate. The ifelse
  # part is to avoid doing any calculations to the zeros.
  cw.churn[,i] = ifelse(cw.churn[,i] != 0,
                        yes = 1.0 - cw.churn[,i],
                        no = 0 + cw.churn[,i])
}
rm(i) # Remove object i vector from environment
# Cloning the churn mixpanel
churn.avgs = cw.churn
# Converting 0.0000 to NAs
churn.avgs[churn.avgs == 0.0000] = NA
avgs.chu = round(x = apply(X = churn.avgs[,-1],
                           MARGIN = 2,
                           FUN = mean,
                           na.rm = TRUE),
                 digits = 4)
avgs.chu = c(0,avgs.chu) # Add zero for first column
# Adding the averages row to the churn mixpanel
cw.churn = rbind(cw.churn,avgs.chu)

# SAVE COHORT WIDE CHURN DATA INTO CSV FILE
write.csv(x = cw.churn,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/CW Churn Rate.csv',
          row.names = FALSE)

# VISUALIZATION
# Creating 19 breaks and 20 rgb color values ranging from red to white
breaks2 = quantile(cw.churn[,3:13],
                    probs = seq(.05, .95, .05),
                    na.rm = TRUE)
colors2 = sapply(round(seq(255, 40, length.out = length(breaks2) + 1), 0),
                  function(x){ rgb(255,x,x, maxColorValue = 255)})
# The churn rate mixpanel
DT::datatable(cw.churn,
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
  formatStyle(names(cw.churn[c(-1,-2)]),
              color = 'white',
              fontWeight = 'bold',
              backgroundColor = styleInterval(breaks2,colors2))