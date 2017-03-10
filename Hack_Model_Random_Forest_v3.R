


rm(list = ls())




setwd("C:\\Users\\1228918\\Documents\\Hackathon\\Model")


full_data =  read.csv("prep5.csv",header=TRUE,stringsAsFactors = FALSE)

drops <- c("idx","id","yr","binned_y_target6","binned_y_target12")

full_data_v2 = full_data[ , !(names(full_data) %in% drops)]

#full_data_v3 = full_data_v2
full_data_v3 <- na.omit(full_data_v2)

str(full_data_v3)




for(i in 1:ncol(full_data_v3)){
  full_data_v3[, i] <- as.factor( full_data_v3[, i])
}




smp_size <- floor(0.7 * nrow(full_data_v3))


set.seed(1234)
train_ind <- sample(seq_len(nrow(full_data_v3)), size = smp_size)


train_data <- full_data_v3[train_ind, ]
test_data <- full_data_v3[-train_ind, ]


# Random forest
library(randomForest)


mtrys = c(2,3,4,5,6,7,8,9)
mxnds = c(8,16,32,64,128)
ntrees = c(1000,1500,2000,5000,10000)
finalacc = 0

for (mtr in mtrys) {
  for (mxnd in mxnds) {
    for (ntr in ntrees) {
      rf_fit = randomForest(binned_y_target1 ~ .,data = train_data,mtry=mtr,maxnodes=mxnd,ntree=ntr)
      rf_probs = predict(rf_fit,test_data,type = "response")
      
      test_data$rfprobs = rf_probs
      
      tble = table(test_data$rfprobs,test_data$binned_y_target1)
      
      acc = (tble[1,1]+tble[2,2])/sum(tble)
      acc
      if (acc > finalacc){
        print(paste("No.of vars",mtr,"Max node",mxnd,"Num tree",ntr,"Accry",acc))
        finalacc = acc
      }
      
    }
    
  }  
}

#[1] "No.of vars 2 Max node 8 Num tree 1000 Accry 0.928104575163399"
#[1] "No.of vars 9 Max node 16 Num tree 1000 Accry 0.934640522875817"


#tble




rf_fit = randomForest(binned_y_target1 ~ .,data = train_data,mtry=9,maxnodes=16,ntree=1000)
rf_probs = predict(rf_fit,test_data,type = "response")

test_data$rfprobs = rf_probs

tble = table(test_data$rfprobs,test_data$binned_y_target1)


tble = table(test_data$binned_y_target1,test_data$rfprobs)
tble

table(test_data$rfprobs)
table(test_data$binned_y_target1)

acc = (tble[1,1]+tble[2,2])/sum(tble)
acc






