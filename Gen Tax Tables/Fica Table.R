library(XML)
# install.packages("RCurl")
library(RCurl)


url="http://www.controller.iastate.edu/payroll/fica.htm"
theurl <- getURL(url)
tables <- readHTMLTable(theurl)
names(tables)

r=adt(tables[[2]])
r
colnames(r)=av(unlist(r[1,]))
r=r[-1,]
x=r
r=lapply(as.vector(unlist(x[[2]])),function(x) strsplit(x,"")[[1]])
r=lapply(r,function(x) x[x!="+"])
r=lapply(r,function(x) x[x!="$"])
r=lapply(r,function(x) x[x!=","])
r=lapply(r,function(x) paste(x,collapse = ""))
x[[2]]=r
r=lapply(as.vector(unlist(x[[3]])),function(x) strsplit(x,"")[[1]])
r=lapply(r,function(x) x[x!="+"])
r=lapply(r,function(x) x[x!="$"])
r=lapply(r,function(x) x[x!="%"])
r=lapply(r,function(x) x[x!=","])
r=lapply(r,function(x) as.numeric(paste(x,collapse = ""))/100)
x[[3]]=r
r=lapply(as.vector(unlist(x[[4]])),function(x) strsplit(x,"")[[1]])
r=lapply(r,function(x) x[x!="+"])
r=lapply(r,function(x) x[x!="$"])
r=lapply(r,function(x) x[x!=","])
r=lapply(r,function(x) paste(x,collapse = ""))
x[[4]]=r
ind=ul(lapply(x[["Medicare"]],function(x) length(strsplit(ac(x),"\n")[[1]])>1))
x[["Medicare"]]=as.character(x[["Medicare"]])
mode(x[["Medicare"]])="character"
ind2=setdiff(seq(1,length(ind)),which(ind))
r=lapply(as.vector(unlist(x[[5]][ind2])),function(x) strsplit(ac(x),"")[[1]])
r=lapply(r,function(x) x[x!="+"])
r=lapply(r,function(x) x[x!="$"])
r=lapply(r,function(x) x[x!="%"])
r=lapply(r,function(x) x[x!=","])
r=lapply(r,function(x) as.numeric(paste(x,collapse = ""))/100)
x[ind2,][["Medicare"]]=unlist(r)
x[["Phase2_Medicare_Threshold"]]=0
x[["Phase2_Medicare_Rate"]]=0
t1=lapply(x[ind,][["Medicare"]],function(x) strsplit(x,"%")[[1]][2])
t2=lapply(t1,function(x) strsplit(x,"\\$")[[1]][2])
t3=lapply(t2,function(x) strsplit(x,"")[[1]])
t3=lapply(t3,function(x) x[x!=","])
x[["Phase2_Medicare_Threshold"]][ind]=lapply(t3,function(x) paste0(x[1:6],collapse=""))
x[["Phase2_Medicare_Rate"]][ind]=lapply(t3,function(x) as.numeric(paste0(x[(length(x)-3):length(x)],collapse=""))/100)
x[ind,][["Medicare"]]=an(unlist(lapply(x[ind,][["Medicare"]],function(x) strsplit(x,"%")[[1]][1])))/100
mode(x[[2]])="numeric"
mode(x[[3]])="numeric"
mode(x[[4]])="numeric"
mode(x[[5]])="numeric"
mode(x[[6]])="numeric"
mode(x[[7]])="numeric"
colnames(x)[1]="Tax_Year"


colnames(x)[2:4]=c("FICA_Base", "FICA_Rate", "FICA_Max" )
x[["Source"]]=url
setwd("~/Taxes/Tax Tables")
wx(x,"FICA Table")


