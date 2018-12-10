setwd("~/Taxes/Input Tables/Template")
files=setdiff(list.files(),"Alec")
files=setdiff(list.files(),"gen inputs.R")
rm(y)
hourly2=seq(10,80,10)
hours2=seq(40,160,20)
hourly2
hours2


for (f in files){
  print(f)
  s=strsplit(f,"\\ ")[[1]][1]
  hourly=strsplit(f,"\\ ")[[1]][2]
  hours=strsplit(f,"\\ ")[[1]][3]
  it=strsplit(f,"\\ ")[[1]][4]
  setwd("~/Taxes/Input Tables/Template")
  x=read.xlsx(paste(f))
  for (v in hourly2){
    for (h in hours2){
      if (it=="1099.xlsx"){
        x[["Contractor_Rate"]]=v
        x[["Contractor_Hours"]]=h
        x[["Contractor_Wage"]]=v*h
      } else {
        x[["W2_Rate"]]=v
        x[["W2_Hours"]]=h
        x[["W2_Wage"]]=v*h
      }
      setwd("~/Taxes/Input Tables")
      fn=paste(s,v,h,it)
      wx(x,strsplit(fn,"\\.")[[1]][1])
    }
  }
}
