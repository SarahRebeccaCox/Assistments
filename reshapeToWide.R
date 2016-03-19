library(tidyr)
library(dplyr)

data <- read.csv("cleanerData.csv")

#chop off index column
data <- data[,-1]

#only keep data with opportunity < 100
data.opportunity.cutoff <- data[data$opportunity <= 100,]
#data.wide <- reshape(data.opportunity.cutoff, idvar="user_id", timevar="opportunity", direction="wide")

#split data by skill
data.split <- split(data.opportunity.cutoff,data.opportunity.cutoff$skill_id)
length(data.split)


#reshape each new dataframe into wide form
for (i in 1:length(data.split)){
  assign(paste("skill",i,sep="."),reshape(data.split[[i]], idvar="user_id", timevar="opportunity", direction="wide"))
}

#how to recombine?

