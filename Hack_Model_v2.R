


rm(list = ls())



setwd("D:/Bhupinder/Projects/2017/POCs/11_Hackathon")
file_name = "final_dataset.csv"
file_name = "result4.csv"

full_data =  read.csv(file_name,header=TRUE,stringsAsFactors = TRUE)
names(full_data)


#[1] "doc_id"                          "senti_score_assets"              "senti_score_economy"            
#[4] "senti_score_expenses"            "senti_score_govt.policy"         "senti_score_industry"           
#[7] "senti_score_innovation"          "senti_score_liquidity"           "senti_score_margins"            
#[10] "senti_score_merges_acquisitions" "senti_score_negative.indicators" "senti_score_positive.indicators"
#[13] "senti_score_pricing"             "senti_score_product.portfolio"   "senti_score_ratio"              
#[16] "senti_score_recent.trends"       "senti_score_recognition_awards"  "senti_score_revenue"            
#[19] "senti_score_stocks"              "company"                         "BSE_Code"                       
#[22] "Release_Date"                    "BSE_Code_Yr"                     "id"                             
#[25] "yr"                              "Base_Price"                      "one_mnthcp"                     
#[28] "six_mnthcp"                      "one_yearcp"                      "one_mnthcpchngper"              
#[31] "six_mnthcpchngper"               "one_yearcpchngper"    


drops <- c("doc_id","company","BSE_Code","Release_Date","BSE_Code_Yr","id","yr","Base_Price",
           "one_mnthcp","six_mnthcp","one_yearcp", "senti_score_negative.indicators",
           "senti_score_positive.indicators", "idx")

drops <- c("y_target",	"BSE_Code",	"Release_Date",	"BSE_Code_Yr",	"company")

full_data_v2 = full_data[ , !(names(full_data) %in% drops)]

modefunc <- function(x){
  tabresult <- tabulate(x)
  themode <- which(tabresult == max(tabresult))
  if(sum(tabresult == max(tabresult))>1) themode <- NA
  return(themode)
}


# for(i in 1:ncol(full_data_v2)){
#   full_data_v2[, i] <- as.factor(full_data_v2[, i])
# }

#full_data_v2<-apply(full_data_v2, 2, modefunc)
full_data_v3 <- na.omit(full_data_v2)

attach(full_data_v3)

smp_size <- floor(0.7 * nrow(full_data_v2))

#binning
senti_cols <- c("senti_score_assets","senti_score_economy", "senti_score_expenses" ,
                "senti_score_govt.policy", "senti_score_industry" ,
                "senti_score_innovation", "senti_score_liquidity" ,
                "senti_score_margins" ,"senti_score_merges_acquisitions",
                "indicators_ratio", "senti_score_pricing" , "senti_score_product.portfolio",  
                "senti_score_ratio", "senti_score_recent.trends",     
                "senti_score_recognition_awards", "senti_score_revenue",           
                "senti_score_stocks")


#full_data_v2[paste("binned_", col, sep='')] <- cut(full_data_v2[,col], c(0,5,10,15,20))

set.seed(123)
train_ind <- sample(seq_len(nrow(full_data_v3)), size = smp_size)


train_data <- full_data_v3[train_ind, ]
test_data <- full_data_v3[-train_ind, ]



# # Linear Regression
# lm_fit_1yr = lm(one_yearcpchngper ~ .-six_mnthcpchngper-one_mnthcpchngper,data = train_data)
# summary(lm_fit_1yr)
# 
# 
# # Linear Regression
# lm_fit_6m = lm(six_mnthcpchngper ~ .-one_yearcpchngper-one_mnthcpchngper,data = train_data)
# summary(lm_fit_6m)
# 
# #lm_fit_1m = lm(one_mnthcpchngper  ~ .-one_yearcpchngper-six_mnthcpchngper,data = train_data)
# lm_fit_1m = lm(y_target  ~ .-one_mnthcpchngper-one_yearcpchngper-six_mnthcpchngper,data = train_data)
# summary(lm_fit_1m)


library(rpart)
library(rpart.plot)
dtree = rpart(y_segment  ~ .,data = train_data)
plot(dtree)
text(dtree, use.n=TRUE, cex=.8, xpd=NA) # cex is a guess, depends on your window size

lmodel = lm(y_segment ~ ., data = train_data)

summary(lmodel)

# Random forest
library(randomForest)


mtrys = c(2,3,4,5,6,7,8,9)
mxnds = c(8,16,32,64,128)
ntrees = c(1000,1500,2000,5000,10000)
finalacc = 0

for (mtr in mtrys) {
  for (mxnd in mxnds) {
    for (ntr in ntrees) {
rf_fit = randomForest(y_segment ~ .,data = train_data,mtry=mtr,maxnodes=mxnd,ntree=ntr)
rf_probs = predict(rf_fit,test_data,type = "response")

test_data$rfprobs = rf_probs

tble = table(test_data$rfprobs,test_data$y_segment)

acc = (tble[1,1]+tble[2,2])/sum(tble)
acc
if (acc > finalacc){
  print(paste("No.of vars",mtr,"Max node",mxnd,"Num tree",ntr,"Accry",acc))
  finalacc = acc
}

  }
  
  }  
}


# 
# 
# library(e1071)
# empdf = data.frame()
# 
# costs = c(0.001,0.1,1,2,3,4,5,10,15)
# plys = c(1,2,3,4,5,6)
# 
# i=1
# 
# for (cst in costs){
#   for (ply in plys){
#     svm_fit = svm(one_yearcpchngper ~ .-six_mnthcpchngper-one_mnthcpchngper,data = train_data,kernel="poly",cost=cst,degree=ply,scale = TRUE)
#     svm_pred = predict(svm_fit,test_data)
#     
#     R2 <- 1 - (sum((test_data$one_yearcpchngper-svm_pred )^2)/sum((test_data$one_yearcpchngper-mean(test_data$one_yearcpchngper))^2))
#     
#     print(paste(cst,ply,R2))
#   }
# }
# 
# 
# 
# names(empdf)<-c("Cost","Degree","Accuracy")
# 
# 
# 











