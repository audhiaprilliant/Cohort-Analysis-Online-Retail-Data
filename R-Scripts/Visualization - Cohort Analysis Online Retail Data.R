# ===== VISUALIZATION - COHORT ANALYSIS =====

library(ggplot2)

# READ THE DATA
cw.retention = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Cohort Analysis - Online Retail Data/Datasets/Total Customer/3 cohort2011 Wide Data - Total Customer.csv',
                        header = TRUE,
                        sep = ',')
View(cw.retention)

# PERCENTAGES OF CUSTOMER EACH MONTHS
data.month = data.frame(Cohort = cw.retention$Cohort,
                   Perc = round(x = (cw.retention$X0/sum(cw.retention$X0)),
                                digits = 3))
data.month = as.data.frame(data.month)
View(data.month)
ggplot(data.month)+
  geom_bar(aes(x = letters[1:dim(data.month)[1]],
               y = Perc),
           fill = I('red'),
           stat = 'identity',
           col = I('white'),
           alpha = 0.7)+
  geom_line(aes(x = 1:dim(data.month)[1],
                y = Perc),
            size = 1,
            col = I('black'))+
  geom_point(aes(x = 1:dim(data.month)[1],
                 y = Perc),
             size = 2,
             col = I('blue'))+
  geom_text(aes(x = letters[1:dim(data.month)[1]],
                y = (Perc),
                label = Perc),
            position = position_dodge(0.9),
            vjust = -1,
            size = 3)+
  labs(title = 'Percentages of First Purchased Customer',
       subtitle = 'Online Retail Data',
       caption = 'Customer in 2011')+
  scale_x_discrete(labels = substr(x = data.month$Cohort,
                                   start = 1,
                                   stop = 3))+
  xlab('Cohort')+
  ylim(c(0,0.21))+
  theme_bw()
