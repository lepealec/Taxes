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
rm(yx2);tax_years=seq(2015,2018)
for (tax_year in tax_years){
  url=paste0("https://taxfoundation.org/",tax_year,"-tax-brackets/",collapse="")
  entity="Federal"
  theurl <- getURL(url)
  tables <- readHTMLTable(theurl)
  ind=agrep("Standard Deduction",names(tables))
  if (length(ind)!=1){
    sys
  }
  x=tables[[ind]]
  r=lapply(as.vector(unlist(x[[2]])),function(x) strsplit(x,"")[[1]])
  r=lapply(r,function(x) x[x!="+"])
  r=lapply(r,function(x) x[x!="$"])
  r=lapply(r,function(x) x[x!=","])
  r=lapply(r,function(x) paste(x,collapse = ""))
  x=cbind(av(unlist(x[[1]])),unlist(r))
  rownames(x)=NULL
  y=cbind(entity,tax_year,x)
  colnames(y)=c("Entity","Tax_Year","Filing_Status","Standard_Deduction")
  y=adt(y)
  y[["Source"]]=url
  if (exists('yx2')){
    yx2=rbind(yx2,y)
  } else {
    yx2=y
  }
}
yx2=ann(adt(yx2))

yx2=adt(cbind(yx2[["Entity"]],yx2[["Filing_Status"]],yx2[["Tax_Year"]],yx2[["Standard_Deduction"]],yx2[["Source"]]))
colnames(yx2)=c("Entity","Filing_Status","Tax_Year","Standard_Deduction","Source")
yx2[yx2[["Filing_Status"]]=="Married Filing Jointly",][["Filing_Status"]]="MFJ"
yx2[yx2[["Filing_Status"]]=="Head of Household",][["Filing_Status"]]="HOH"


tax_years=seq(2015,2018)
tax_year=2016
for (tax_year in tax_years){
  url=paste0("https://www.ftb.ca.gov/forms/",tax_year,"-California-Tax-Rates-and-Exemptions.shtml",collapse="")
  theurl <- getURL(url)
  tables <- readHTMLTable(theurl)
  entity="CA"
  
  r=(tables)[[4]]
  if (length(agrep("Deduction",colnames(r)))!=1){
    sys
  }
  r[[2]]=gsub("\\$","",as.vector(r[[2]]))
  r[[2]]=gsub("\\,","",as.vector(r[[2]]))
  y=cbind("Entity"=entity,as.character(r[[1]]),"Tax_Year"=as.numeric(tax_year),r[[2]])
  y=adt(y)
  y[["Source"]]=url
  yx2=rbindlist(list(yx2,y))
}


yx2[yx2[["Filing_Status"]]=="Single or married/RDP filing separately",][["Filing_Status"]]="Single/MFS"
yx2[yx2[["Filing_Status"]]=="Married/RDP filing jointly, head of household, or qualifying widow(er)",][["Filing_Status"]]="HOH/MFJ/QWD"
yx2[yx2[["Filing_Status"]]=="The minimum standard deduction for dependents",][["Filing_Status"]]="Dependent"


tax_years=seq(2015,2018)
rm(yx3)
for (tax_year in tax_years){
  url=paste0("https://www.ftb.ca.gov/forms/",tax_year,"-California-Tax-Rates-and-Exemptions.shtml",collapse="")
  theurl <- getURL(url)
  tables <- readHTMLTable(theurl)
  entity="CA"
  
  r=(tables)[[2]]
  if (length(agrep("Exemption",colnames(r)))!=1){
    sys
  }
  r[[2]]=gsub("\\$","",as.vector(r[[2]]))
  r[[2]]=gsub("\\,","",as.vector(r[[2]]))
  y=cbind("Entity"=entity,as.character(r[[1]]),"Tax_Year"=as.numeric(tax_year),r[[2]])
  y=adt(y)
  colnames(y)=c("Entity","Filing_Status","Tax_Year","Exemption_Credit")
  y[["Source"]]=url
  if (exists('yx3')){
    yx3=rbindlist(list(yx3,y))
  } else {
    yx3=y
  }
}
yx3[yx3[["Filing_Status"]]=="Single, married/RDP filing separately, or head of household",][["Filing_Status"]]="Single/HOH/MFS"
yx3[yx3[["Filing_Status"]]=="Married/Registered Domestic Partner (RDP) filing jointly or qualifying widow(er)",][["Filing_Status"]]="MFJ"

temp=yx3
rm(out)
i=2
Correct_Slashes=function(temp){
  for (i in seq(1,nrow(temp))){
    rd=temp[i,]
    r=strsplit(rd[["Filing_Status"]],"/")[[1]]
    log1=length(r)>=2
    if (log1){
      temp_out=matrix(ncol=ncol(temp),nrow=0)
      for (q in r){
        if ("Exemption_Credit" %in% colnames(rd)){
          temp_out=rbind(temp_out,c(rd[["Entity"]],q,rd[["Tax_Year"]],rd[["Exemption_Credit"]],rd[["Source"]]))
        } else {
          temp_out=rbind(temp_out,c(rd[["Entity"]],q,rd[["Tax_Year"]],rd[["Standard_Deduction"]],rd[["Source"]]))
        }
      }
      y=adt(temp_out)
      colnames(y)=colnames(temp)
    } else {
      y=rd
    }
    if (exists('out')){
      out=rbindlist(list(out,y))
    } else {
      out=y
    }
  }
  return(out)
}

yx3=Correct_Slashes(yx3)
yx2=Correct_Slashes(yx2)
Standard_Deductions=yx2
temp=Standard_Deductions[Standard_Deductions[["Filing_Status"]]=="Single",]
temp[["Filing_Status"]]="MFS"
Standard_Deductions=rbindlist(list(Standard_Deductions,temp))
Standard_Deductions=Standard_Deductions[,!"V1"]
Standard_Deductions=unique(Standard_Deductions)

list.files()

wx(Standard_Deductions,"Standard Deductions")
wx(yx3,"California Exemption Credits")



