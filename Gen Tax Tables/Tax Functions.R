options(scipen=999)
setwd("~/Taxes/Tax Tables")
Calculate_Taxes=function(Entity,Filing_Status,Tax_Year,Taxable_Income,Remove_Zeros=F,Sum_Tax=T){
  ### Examples ###
  # Calculate_Taxes(Entity="Federal",Filing_Status="Single",Tax_Year="2018",Taxable_Income=100000)
  # Calculate_Taxes(Entity="Federal",Filing_Status="Single",Tax_Year="2017",Taxable_Income=100000)
  # Calculate_Taxes(Entity="Federal",Filing_Status="Single",Tax_Year="2016",Taxable_Income=100000)
  # sum(Calculate_Taxes(Entity="Federal",Filing_Status="Single",Tax_Year="2016",Taxable_Income=100000)[["Tax"]])
  # sum(Calculate_Taxes(Entity="Federal",Filing_Status="Single",Tax_Year="2017",Taxable_Income=100000)[["Tax"]])
  # sum(Calculate_Taxes(Entity="Federal",Filing_Status="Single",Tax_Year="2018",Taxable_Income=100000)[["Tax"]])
  # Calculate_Taxes(Entity="CA",Filing_Status="Single",Tax_Year="2016",Taxable_Income=2000000)
  # Calculate_Taxes(Entity="CA",Filing_Status="Single",Tax_Year="2017",Taxable_Income=2000000)
  # Calculate_Taxes(Entity="CA",Filing_Status="Single",Tax_Year="2018",Taxable_Income=2000000)
  ### Examples ###
  if (Entity=="CA" & Filing_Status=="MFS"){
    Filing_Status="Single"
  }
  ind=which(Tax_Table[["Entity"]]==Entity & Tax_Table[["Filing_Status"]]==Filing_Status & Tax_Table[["Tax_Year"]]==Tax_Year)
  Tax_Table_SS=ann(Tax_Table[ind,])
  Taxable_Income=an(Taxable_Income)
  if (nrow(Tax_Table_SS)==0){
    print("Tax Table Not Found")
    sys
  }
  Tax_Table_SS[["Taxable_Income"]]=Taxable_Income
  Tax_Table_SS=ann(Tax_Table_SS)
  Tax_Table_SS[["Taxable_Income"]][Tax_Table_SS[["Taxable_Income"]]<=Tax_Table_SS[["Lower"]]]=0
  yx=Tax_Table_SS[["Taxable_Income"]]>Tax_Table_SS[["Lower"]] & Tax_Table_SS[["Taxable_Income"]]<=Tax_Table_SS[["Upper"]]
  if (sum(yx)>=1){
    Tax_Table_SS[["Taxable_Income"]][yx]=Tax_Table_SS[["Taxable_Income"]][Tax_Table_SS[["Taxable_Income"]]>Tax_Table_SS[["Lower"]] & Tax_Table_SS[["Taxable_Income"]]<=Tax_Table_SS[["Upper"]]]-Tax_Table_SS[["Lower"]][Tax_Table_SS[["Taxable_Income"]]>Tax_Table_SS[["Lower"]] & Tax_Table_SS[["Taxable_Income"]]<=Tax_Table_SS[["Upper"]]]
  }
  yx=Tax_Table_SS[["Taxable_Income"]]>Tax_Table_SS[["Upper"]]
  if (sum(yx)>=1){
    Tax_Table_SS[["Taxable_Income"]][yx]=
      Tax_Table_SS[["Upper"]][Tax_Table_SS[["Taxable_Income"]]>Tax_Table_SS[["Upper"]]]-Tax_Table_SS[["Lower"]][Tax_Table_SS[["Taxable_Income"]]>Tax_Table_SS[["Upper"]]]
  }
  Tax_Table_SS=ann(Tax_Table_SS)
  Tax_Table_SS[["Tax"]]=Tax_Table_SS[["Tax_Rate"]]*Tax_Table_SS[["Taxable_Income"]]
  assign('Tax_Table_SS',Tax_Table_SS,envir=.GlobalEnv)
  assign('Taxable_Income',Taxable_Income,envir=.GlobalEnv)
  
  if (abs(sum(Tax_Table_SS[["Taxable_Income"]])-Taxable_Income)>500){
    assign('Tax_Table_SS',Tax_Table_SS,envir=.GlobalEnv)
    assign('Taxable_Income',Taxable_Income,envir=.GlobalEnv)
    
    print("Taxable Income Error")
    sys
  }
  if (Remove_Zeros){
    Tax_Table_SS=Tax_Table_SS[Tax_Table_SS[["Tax"]]!=0,]
  }
  if (Sum_Tax==T){
    return(sum(Tax_Table_SS[["Tax"]]))
  } else {
    return(Tax_Table_SS)
  }
}

Calculate_FICA_Taxes=function(Tax_Year,Taxable_Income,Filing_Status="S",Type=""){
  ### Exmaple ###
  # Calculate_FICA_Taxes(2010,100000)
  # Calculate_FICA_Taxes(2010,200000)
  # Calculate_FICA_Taxes(2010,250000)
  ### Example ###
  Taxable_Income=as.numeric(Taxable_Income)
  yx=which(FICA_Table[["Tax_Year"]]==Tax_Year)
  FICA_Table_SS=FICA_Table[yx,]
  if (nrow(FICA_Table_SS)!=1){
    print("FICA Error")
    sys
  }
  if (Filing_Status=="MFJ"){
    FICA_Table_SS[["FICA_Base"]]=FICA_Table_SS[["FICA_Base"]]*2
    FICA_Table_SS[["FICA_Max"]]=FICA_Table_SS[["FICA_Max"]]*2
    FICA_Table_SS[["Phase2_Medicare_Threshold"]]=FICA_Table_SS[["Phase2_Medicare_Threshold"]]*2
  }
  FICA_Table_SS=ann(FICA_Table_SS)
  SS_Tax=min(Taxable_Income*FICA_Table_SS[["FICA_Rate"]],FICA_Table_SS[["FICA_Max"]])
  Medicare_Tax=FICA_Table_SS[["Medicare"]]*(min(Taxable_Income,FICA_Table_SS[["Phase2_Medicare_Threshold"]]))
  Medicare_Phase2_Tax=FICA_Table_SS[["Phase2_Medicare_Rate"]]*(max(Taxable_Income-FICA_Table_SS[["Phase2_Medicare_Threshold"]],0))
  amp=1
  out=list(amp*SS_Tax,amp*(Medicare_Phase2_Tax+Medicare_Tax))
  names(out)=c("Social_Security_Tax","Total_Medicare_Tax")
  return(out)
}
simple_left_merge=function(x,y,by,variable,variable_name=NA){
  x[["I"]]=seq(1,nrow(x))
  for (i in colnames(y)){
    if (!(i %in% c(by,variable))){
      y=y[,!i,with=F]
    }
  }
  out=merge(x,y,by,all.x=T)
  if (!is.na(variable_name)){
    colnames(out)[ncol(out)]=variable_name
  }
  out=out[order(out[["I"]]),]
  out=out[,!"I",with=F]
  if (sum(is.na(out[[ncol(out)]]))>=1){
    out[is.na(out[[ncol(out)]]),][[ncol(out)]]=0
  }
  return(out)
}
cumsum_year=function(x,var="Contractor_Net_Revenue"){
  y=adt(cbind(x[["Calendar_Year"]],x[["Calendar_Month"]],x[["Month_N"]],x[[var]]))
  y=ann(y)
  colnames(y)=c("Calendar_Year","Calendar_Month","Month_N",var)
  yrs=unique(y[["Calendar_Year"]])
  for (yr in yrs){
    temp=y[y[["Calendar_Year"]]==yr,]
    temp[[paste0(var,"_CS",collapse = "")]]=cumsum(temp[[var]])
    if (exists('out')){
      out=rbindlist(list(out,temp))
    } else {
      out=temp
    }
  }
  x[[paste0(var,"_CS",collapse = "")]]=out[[paste0(var,"_CS",collapse = "")]]
  return(x)
}
tax_apply=function(x,var,f="",type=""){
  temp=ann(adt(cbind(x[["Tax_Year"]],x[[var]],x[["Filing_Status"]])))
  if (f=="Calculate_FICA_Taxes" & type=="SE"){
    x[["Contractor_SS_Tax"]]=apply(temp,1,function(x) Calculate_FICA_Taxes(x[1],x[2],x[3],"SE")$Social_Security_Tax)*2
    x[["Contractor_ME_Tax"]]=apply(temp,1,function(x) Calculate_FICA_Taxes(x[1],x[2],x[3],"SE")$Total_Medicare_Tax)*2
  }
  if (f=="Calculate_FICA_Taxes" & type=="Total"){
    x[["Total_SS_Tax"]]=apply(temp,1,function(x) Calculate_FICA_Taxes(x[1],x[2],x[3])$Social_Security_Tax)*2
    x[["Total_ME_Tax"]]=apply(temp,1,function(x) Calculate_FICA_Taxes(x[1],x[2],x[3])$Total_Medicare_Tax)*2
  }
  if (f=="Calculate_FICA_Taxes" & type==""){
    x[["SS_Tax"]]=apply(temp,1,function(x) Calculate_FICA_Taxes(x[1],x[2],x[3])$Social_Security_Tax)*2
    x[["ME_Tax"]]=apply(temp,1,function(x) Calculate_FICA_Taxes(x[1],x[2],x[3])$Total_Medicare_Tax)*2
  }
  # if (f=="Calculate_Taxes"){
  #   x[[paste0(type,"_Tax",collapse="")]]=apply(temp,1,function(x) Calculate_Taxes(x[1],x[2]))
  # }
  return(x)
  
}

save(Calculate_Taxes,Calculate_FICA_Taxes,simple_left_merge,cumsum_year,tax_apply,file="Tax_Functions.RData")
