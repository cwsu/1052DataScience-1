########################
# homework1 example
########################

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_105753008.R  -files test.1.csv -out result.csv", call.=FALSE)
} else if (length(args)==1) {
  i_f <- args[1] 
}

	# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "-files"){
    i_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "-out"){
    o_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}
name <- i_f

	#Read in a input file--test.1.csv
data <- read.csv(i_f)

	#max
weightmax <- apply(data[2],2,max)
heightmax <- apply(data[3],2,max)
weightmax2 <- round(weightmax, digits = 2)
heightmax2 <- round(heightmax, digits = 2)


	#Output summary file--result.csv
out <- data.frame(set=name,weight=weightmax2,height=heightmax2)
write.csv(out, file = o_f, row.names = FALSE)
