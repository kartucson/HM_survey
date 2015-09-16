library(Hmisc)
library(car)
setwd("C:\\Users\\karthik\\Google Drive\\Casey\\HM Survey\\Data_analysis")

data_in <- read.csv("quantitative_data.csv")
summary(data_in)
str(data_in)

aggregate(data_in$Gender_Text, by=list(data_in$Gender_Text), FUN = function(x){NROW(x)})

data_num <- data_in[,!colnames(data_in) %in% c('Gender_Text','Region')]

data_nu <- round(impute(data_num,mean),2)

## Since only age seems to have much of the na values, it may be better to do simple imputation ##

round(cor(data_nu),2)

data_sc <- na.omit(data_num) # Loose 30% data

write.csv(as.data.frame(round(rcorr(as.matrix(data_sc),type = "pearson")$P,4)),"var_cor_p_values.csv")
write.csv(as.data.frame(round(rcorr(as.matrix(data_sc),type = "pearson")$r,4)),"var_cor.csv")

scatterplotMatrix((data_nu, var.labels=colnames(x), 
                   diagonal=c("density", "boxplot", "histogram", "oned", "qqplot", "none")
                   
scatterplotMatrix(~ income + education + prestige, 
                 transform=TRUE, data=Duncan, smoother=loessLine)                   

