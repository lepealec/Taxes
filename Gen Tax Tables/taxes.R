setwd
options(scipen=999)
#dir.create("Tax Tables")
setwd("Q:/Alec/Base")
load("Base.RData");lib()
setwd("~/Taxes/Tax Tables")
# library("rvest")
library(XML)
# install.packages("RCurl")
library(RCurl)
# library(rlist)

url="https://www.tax-brackets.org/federaltaxtable/2019"
theurl <- getURL(url)
tables <- readHTMLTable(theurl)

tax_year=2018
rm(yx)
tax_years=seq(2010,2018)
for (tax_year in tax_years){
  url=paste0("https://www.tax-brackets.org/federaltaxtable/",tax_year+1,collapse="")
  entity="Federal"
  theurl <- getURL(url)
  tables <- readHTMLTable(theurl)
  names(tables)=c("Single","MFS","MFJ","HOH")
  tables=tables[1:4]
  for (i in seq(1,length(tables))){
    r=sapply(as.vector(unlist(tables[[i]][1])),function(x) strsplit(x,"")[[1]])
    r=lapply(r,function(x) x[x!="+"])
    r=lapply(r,function(x) x[x!="$"])
    r=lapply(r,function(x) x[x!=","])
    r=lapply(r,function(x) paste(x,collapse = ""))
    g=lapply(as.vector(unlist(tables[[i]][2])),function(x) strsplit(x,"")[[1]])
    g=lapply(g,function(x) x[x!="%"])
    g=lapply(g,function(x) x[x!="."])
    g=lapply(g,function(x) c("0.",x))
    g=lapply(g,function(x) paste(x,collapse = ""))
    x=cbind(unlist(r),unlist(g))
    rownames(x)=NULL
    y=matrix(ncol=3,nrow=0)
    for (i2 in seq(1,nrow(x))){
      if (i2!=nrow(x)){
        y=rbind(y,c(x[i2,1],x[i2+1,1],x[i2,2]))
      } else {
        y=rbind(y,c(x[i2,1],as.numeric(x[i2,1])*10,x[i2,2]))
      }
    }
    y=cbind(entity,names(tables)[[i]],tax_year,y)
    if (is.na(names(tables)[[i]])){
      sys
    }
  #  y=adt(y)
    y=cbind(y,url)
    colnames(y)=c("Entity","Filing Status","Tax Year","Lower","Upper","Tax Rate","Source")
    if (exists('yx')){
      yx=rbind(yx,y)
    } else {
      yx=y
    }
  }
}

mode(yx[,"Filing Status"])="character"
mode(yx[,"Tax Rate"])="character"

tax_years=seq(2016,2019)
tax_year=2016
for (tax_year in tax_years){
  url=paste0("https://www.ftb.ca.gov/forms/",tax_year,"-California-Tax-Rates-and-Exemptions.shtml",collapse="")
  theurl <- getURL(url)
  tables <- readHTMLTable(theurl)
  entity="CA"
  fss=c("Single","MFJ","HOH")
  schs=c("Schedule X","Schedule Y","Schedule Z")
  for (fs in fss){
    single=agrep(schs[which(fs==fss)],names(tables),max.distance =0)
    if (length(single)==1){
      y=tables[[single]]
      y[[1]]=gsub("\\$","",as.vector(y[[1]]))
      y[[1]]=gsub("\\,","",as.vector(y[[1]]))
      y[[2]]=gsub("\\$","",as.vector(y[[2]]))
      y[[2]]=gsub("\\,","",as.vector(y[[2]]))
      if (sum(y[[2]]=="AND OVER")>=1){
        y[y[[2]]=="AND OVER",][[2]]=9999999
      }
      if (sum(y[[2]]=="and over")>=1){
        y[y[[2]]=="and over",][[2]]=9999999
      }
      r=strsplit(as.character(y[[3]]),"%")
      r=lapply(r,function(x) x[1])
      r=lapply(r,function(x) strsplit(x,"")[[1]][(length(strsplit(x,"")[[1]])-6):length(strsplit(x,"")[[1]])])
      
      #r=lapply(r,function(x) x[x!="+"])
      r=lapply(r,function(x) paste(x[(which(x=="+")+1):length(x)],collapse=""))
      r=as.numeric(unlist(r))/100
      y[[3]]=r
      y=cbind(entity,fs,tax_year,y)
      y=cbind(y,url)
      
      colnames(y)=colnames(yx)
      y[,2]=as.vector(y[,2])
      mode(y[,2])="character"
      mode(y[,"Tax Rate"])="character"
      yx=rbind(yx,y)
    }
  }
}




yx=as.data.table(yx)
yx[["Lower"]]=as.numeric(as.vector(yx[["Lower"]]))
yx[["Upper"]]=as.numeric(as.vector(yx[["Upper"]]))
yx[["Tax Rate"]]=as.numeric(as.vector(yx[["Tax Rate"]]))
colnames(yx)[c(2,3,6)]=cc("Filing_Status Tax_Year Tax_Rate")
wx(yx,"Tax_Rates")
Tax_Table=ann(adt(read.xlsx("Tax_Rates.xlsx")))
ind=which(colnames(Tax_Table)=="V1")
Tax_Table=Tax_Table[,!"",with=F]
Tax_Table
Entity="Federal"
Tax_Year=2018
Filing_Status="Single"
Taxable_Income=100000
str(ann(Tax_Table))












#### rcys
Taxable_Income=250000

x=seq(1000,250000,1000)
y=sapply(x,function(x) Calculate_FICA_Taxes(2010,x)[[2]])
y2=sapply(x,function(x) Calculate_FICA_Taxes(2010,x)[[1]])
plot(x,y+y2)

Calculate_FICA_Taxes(2016,1000000)
Taxable_Income=200000

Calculate_Taxes(Entity="CA",Filing_Status="Single",Tax_Year="2018",Taxable_Income=2000000)






Tax_Year=2016
Taxable_Income=200000


