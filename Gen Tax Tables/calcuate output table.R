options(scipen=999)
rm(list=ls())
setwd("Q:/Alec/Base")
load("base.RData")
setwd("~/Taxes/Tax Tables") 
lib()
options(warn=0)
Tax_Table=ann(adt(read.xlsx("Tax_Rates.xlsx")))
Standard_Deductions=ann(adt(read.xlsx("Standard Deductions.xlsx")))
California_Exemption_Credits=ann(adt(read.xlsx("California Exemption Credits.xlsx")))
FICA_Table=ann(adt(read.xlsx("FICA TABLE.xlsx")))
load("Tax_Functions.RData")

setwd("~/Taxes/Input Tables")
files=list.files()

file=files[1]
file=files[400]
files=setdiff(files,"Template")
file="MFJ 70 150 1099.xlsx"
#file="MFJ 70 160 1099.xlsx"
file="MFJ 10 150 1099.xlsx"
for (file in files){
  print(file)
  setwd("~/Taxes/Output Tables")
  # od()
  fn=strsplit(file,"\\.")[[1]][1]
  #loga=strsplit(file," ")[[1]][1]=="MFJ" & strsplit(file," ")[[1]][4]=="1099.xlsx"
  loga=F
  if (!(fn %in% list.files()) | loga){
    # sys
    setwd("~/Taxes/Input Tables")
    x=ann(adt(read.xlsx(file)))
    
    ### FICA Taxes
    x[["Contractor_Net_Revenue"]]=x[["Contractor_Wage"]]-x[["Contractor_Expenses"]]
    x=cumsum_year(x,"Contractor_Net_Revenue")
    x[["Net_Revenue"]]=x[["Contractor_Wage"]]-x[["Contractor_Expenses"]]
    x=cumsum_year(x,"W2_Wage")
    x[["Net_Income"]]=x[["Contractor_Wage"]]-x[["Contractor_Expenses"]]+x[["W2_Wage"]]
    x=cumsum_year(x,"Net_Income")
    x=tax_apply(x,"Contractor_Net_Revenue_CS","Calculate_FICA_Taxes","SE")
    x=tax_apply(x,"Net_Income_CS","Calculate_FICA_Taxes",type="Total")
    x[["W2_SS_Tax"]]=(x[["Total_SS_Tax"]]-x[["Contractor_SS_Tax"]])/2
    x[["W2_ME_Tax"]]=(x[["Total_ME_Tax"]]-x[["Contractor_ME_Tax"]])/2
    x[["Contractor_SS_Tax"]]=x[["Contractor_SS_Tax"]]*0.9235
    x[["Contractor_ME_Tax"]]=x[["Contractor_ME_Tax"]]*0.9235
    x=x[,!"Total_SS_Tax",with=F]
    x=x[,!"Total_ME_Tax",with=F]
    x=tax_apply(x,"Net_Income_CS","Calculate_FICA_Taxes",type="Total")
    ### FICA Taxes
    
    ### Federal
    # Standard Deduction
    x=simple_left_merge(x,
                        Standard_Deductions[Standard_Deductions[["Entity"]]=="Federal",],
                        by=c("Tax_Year","Filing_Status"),"Standard_Deduction","FD_SD")
    # Standard Deduction
    # Personal Exemption
    ytemp=Standard_Deductions[Standard_Deductions[["Entity"]]=="Federal" & Standard_Deductions[["Filing_Status"]]=="Personal Exemption",]
    ytemp[["Filing_Status"]]=0
    colnames(ytemp)[which(colnames(ytemp)=="Filing_Status")]="Dependency"
    x=simple_left_merge(x,
                        ytemp,
                        by=c("Tax_Year","Dependency"),"Standard_Deduction","FD_PE")
    # Personal Exemption
    # SE Deduction
    x[["FD_SE_Deduction"]]=(x[["Contractor_SS_Tax"]]+x[["Contractor_ME_Tax"]])/2
    # SE Deduction
    # QBI Simple
    x[["QBI_Simple"]]=x[["Contractor_Net_Revenue_CS"]]*0.2
    if (nrow(x[x[["Tax_Year"]]<=2017,])>=1){
      x[x[["Tax_Year"]]<=2017,][["QBI_Simple"]]=0
    }
    # QBI Simple
    # Taxable Income
    x[["FD_AGI"]]=x[["Net_Income_CS"]]-x[["Deductions"]]-x[["FD_SE_Deduction"]]-x[["QBI_Simple"]]
    x[["FD_TI"]]=x[["Net_Income_CS"]]-x[["FD_PE"]]-x[["FD_SD"]]-x[["Deductions"]]-x[["FD_SE_Deduction"]]-x[["QBI_Simple"]]
    x[["FD_TI"]]=lapply(x[["FD_TI"]],function(x) max(x,0))
    # Taxable Income
    # Federal Income Tax
    x[["FD_Tax"]]=apply(x,1,function(y) Calculate_Taxes("Federal",y[["Filing_Status"]],y[["Tax_Year"]],y[["FD_TI"]]))
    # Federal Income Tax
    ### Federal
    Calculate_Taxes("Federal","Single",2017,56092.567,Sum_Tax=F)
    ### State
    # Standard Deduction
    x=simple_left_merge(x,
                        Standard_Deductions[Standard_Deductions[["Entity"]]=="CA",],
                        by=c("Tax_Year","Filing_Status"),"Standard_Deduction","CA_SD")
    # Standard Deduction
    # California TI and Tax
    x[["CA_TI"]]=sapply(x[["FD_AGI"]]-x[["CA_SD"]],function(x) max(x,0))
    x=simple_left_merge(x,
                        California_Exemption_Credits,
                        by=c("Tax_Year","Filing_Status"),"Exemption_Credit","CA_PE")
    x[["CA_Tax"]]=apply(x,1,function(y) Calculate_Taxes("CA",y[["Filing_Status"]],y[["Tax_Year"]],max(y[["CA_TI"]],0)))
    # California TI and Tax
    # Exemption Credit
    x[["CA_Tax"]]=lapply(x[["CA_Tax"]]-x[["CA_PE"]],function(x) max(x,0))
    # Exemption Credit
    ### State
    x=ann(x)
    for (i in seq(1,ncol(x))){
      if (mode(x[[i]])=='numeric'){
        x[[i]]=round(x[[i]])
      }
    }
    x[["Total_Tax"]]=x[["Contractor_SS_Tax"]]+x[["Contractor_ME_Tax"]]+x[["W2_SS_Tax"]]+x[["W2_ME_Tax"]]+x[["FD_Tax"]]+x[["CA_Tax"]]
    x[["Total_Income"]]=x[["Contractor_Net_Revenue_CS"]]+x[["W2_Wage_CS"]]
    x[x[[1]]==2018,]
    #dir.create("~/Taxes/Output Tables")
    setwd("~/Taxes/Output Tables")
    fn=strsplit(file,"\\.")[[1]][1]
    yrs=unique(x[["Tax_Year"]])
    rm(y)
    dir.create(fn)
    setwd(paste0("~/Taxes/Output Tables/",fn))
    wx(x,paste(fn,"Table"))
    for (yr in yrs){
      temp=x[x[["Tax_Year"]]==yr & x[["Calendar_Month"]]=="December",]
      yy=adt(cbind(yr,temp[["Total_Income"]],temp[["FD_AGI"]],temp[["Total_Tax"]],temp[["Total_Tax"]]/temp[["Total_Income"]],temp[["Total_Income"]]-temp[["Total_Tax"]],temp[["FD_Tax"]],temp[["CA_Tax"]],temp[["Contractor_Net_Revenue_CS"]],temp[["W2_Wage_CS"]],temp[["Contractor_SS_Tax"]],temp[["Contractor_ME_Tax"]],temp[["W2_SS_Tax"]],temp[["W2_ME_Tax"]]))
      colnames(yy)=c("Tax_Year","Total_Income","AGI","Total_Tax","Total_Tax_Rate","Net_Income","Federal_Tax","CA_Tax","Total_Contractor_Income","Total_W2_Income","Total_Contractor_SS_Tax","Total_Contractor_ME_Tax","Total_W2_SS_Tax","Total_W2_ME_Tax")
      if (exists('y')){
        y=rbindlist(list(y,yy))
      } else {
        y=yy
      }
      r=Calculate_Taxes("CA",temp[["Filing_Status"]],temp[["Tax_Year"]],temp[["CA_TI"]],Sum_Tax=F)[,!"",with=F]
      r[["Exemption Credit"]]=temp[["CA_PE"]]
      wx(r,paste(fn,yr,"California Tax Table"))
      wx(Calculate_Taxes("Federal",temp[["Filing_Status"]],temp[["Tax_Year"]],temp[["FD_TI"]],Sum_Tax=F)[,!"",with=F],paste(fn,yr,"Federal Tax Table"))
      wx(FICA_Table[FICA_Table[["Tax_Year"]]==yr,][,!"",with=F],paste(yr,"Social Security Tax Table"))
    }
    y=ann(y)
    wx(y,paste(fn,"Yearly Summary"))
  }
}



setwd("~/Taxes/Output Tables")
ff=list.files()
for (f in ff){
  aswd(c("~/Taxes/Output Tables",f))
  if (length(list.files())!=5){
    od()
    print(f)
  }
}

setwd("Q:/Alec")

