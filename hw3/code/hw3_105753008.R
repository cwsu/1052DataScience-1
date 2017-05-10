library('caret')
library('ROCR')

query_func<-function(query_m, i)
{
  if(query_m == "male"){
    which.min(i)
  }
  else if (query_m == "female") {
    which.max(i)
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw3_105753008.R --target male|female --files meth1 meth2 ¡K methx --out result.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
methodnames<-c()
calsensitivitys<-c()
calspecificitys<-c()
calf1s<-c()
calaucs<-c()
for(file in files)
{
 methodname<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  cal<-c( d$prediction,d$reference,query_m)

  #Create Confusion Matrix
calcon <- confusionMatrix(d$prediction,  d$reference, query_m )

#Sensitivity 
calsensitivity <- calcon$byClass['Sensitivity']

#Specificity 
calspecificity <- calcon$byClass["Specificity"]

#F1
precision <-  calcon$byClass['Pos Pred Value']    
recall <-  calcon$byClass['Sensitivity']
calf1<- 2 * ((precision * recall) / (precision + recall))

#AUC
eval <- prediction(d$pred.score,  d$reference)
plot(performance(eval,"tpr","fpr"))
calauc<-(attributes(performance(eval,'auc'))$y.values[[1]])

 methodnames<-c(methodnames,methodname)
 calsensitivitys<-c(calsensitivitys,calsensitivity)
 calspecificitys<-c(calspecificitys,calspecificity)
 calf1s<-c(calf1s,calf1)
 calaucs<-c(calaucs,calauc)
}
out_data<-data.frame(methodname=methodnames,calsensitivity=calsensitivitys,calspecificity=calspecificitys ,calf1=calf1s,calauc=calaucs,stringsAsFactors = F)
index<-sapply(out_data[,c("calsensitivity","calspecificity","calf1","calauc")], query_func, query_m=query_m)

#count second f1
countmaxf1<-which.max(calf1s)
maxf1<-calf1s[countmaxf1]

delmax1<-calf1s[-countmaxf1]
countsecf1<-which.max(delmax1)
secf1<-calf1s[countsecf1]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
bestfile<-read.csv(paste("hw3/data/set1/method",countmaxf1,".csv",sep=""))
secondfile<-read.csv(paste("hw3/data/set1/method",countsecf1,".csv",sep=""))

#creat contingency table
contingency_table<-table(bestfile$prediction,  secondfile$prediction)
print(contingency_table)

# the null hypothesis : conversion is independent of group
fisher.test(contingency_table)
testp<-fisher.test(contingency_table)$p.value

 if( testp < 0.05) {
   methodname[index[3]] <- paste0(methodname[index[3]], "*")
  }

# output file
out_data <- rbind(out_data,c(query_m,methodname[index]))
write.csv(out_data, file=out_f, row.names = F, quote = F)
out_data 
methodname[index]
