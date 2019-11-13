
nrow(trans_Q2_16_17)

trans_Q2_16_17 <- as.data.frame(trans_Q2_16_17)


remove(Tax_Data_Q1)

Tax_Data_Q1 <-  trans_Q2_16_17[(trans_Q2_16_17$diff_tax <=5000 & trans_Q2_16_17$diff_tax>=-5000  ),]

  write.csv(Tax_Data_Q1,'Tax_Data_Q1.csv')

  
###  For quarter 3 Data ####
  
  
  nrow(trans_Q3_16_17)
  
  trans_Q3_16_17 <- as.data.frame(trans_Q3_16_17)
  
  Tax_Data_Q3 <-  trans_Q3_16_17[(trans_Q3_16_17$diff_tax <=5000 & trans_Q3_16_17$diff_tax>=-5000  ),]
  
  write.csv(Tax_Data_Q3,'Tax_Data_Q3.csv')
  
  
  
  
  