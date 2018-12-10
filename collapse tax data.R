# gen Tax Data.RData
file.rename()
my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}


setwd("Q:/Alec/Base")
load("base.RData") ;lib()
setwd("~/Taxes/Output Tables")
folders=setdiff(list.files(),"Alec")
setwd("~/Taxes")
setwd("~/Taxes/Output Tables")
for (f in folders){
  my.file.rename(paste0(getwd(),"/",f),paste0("C:/Users/alepe/Documents/Tax Data/",f))
}
td="C:/Users/alepe/Documents/Tax Data/"
setwd(td)
folders=setdiff(list.files(),"Alec")
rm(y)
for (f in folders){
  s=strsplit(f,"\\ ")[[1]][1]
  hourly=strsplit(f,"\\ ")[[1]][2]
  hours=strsplit(f,"\\ ")[[1]][3]
  it=strsplit(f,"\\ ")[[1]][4]
  aswd(c("C:/Users/alepe/Documents/Tax Data",f))
  x=read.xlsx(paste(f,"Yearly Summary.xlsx"))
  x[["Status"]]=s
  x[["Hourly"]]=hourly
  x[["Hours"]]=hours
  x[["Income_Type"]]=it
  if (exists('y')){
    y=rbindlist(list(y,x))
  } else {
    y=x
  }
}
y=y[,!"V1"]
y[["ME_Tax"]]=y[["Total_Contractor_ME_Tax"]]+y[["Total_W2_ME_Tax"]]
y[["SS_Tax"]]=y[["Total_Contractor_SS_Tax"]]+y[["Total_W2_SS_Tax"]]
setwd("~/Taxes")
save(y,file="Tax Data.RData")
y=y[order(y[["Status"]],y[["Income_Type"]],y[["Total_Income"]]),]
write.csv(y,"Tax Data.csv")

setwd("~/Taxes/Input Tables")
folders=setdiff(list.files(),"Alec")
dir.create("C:/Users/alepe/Documents/Input Tax Data/")
for (f in folders){
  my.file.rename(paste0(getwd(),"/",f),paste0("C:/Users/alepe/Documents/Input Tax Data/",f))
}
