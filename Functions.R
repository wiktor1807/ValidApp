
#generate test data/imports desired data
importData<-function(){
  contractEndDate<<-"03/31/2035"
  cutoffDate<<-"08/01/2021"
  
  Aircraft_Number <- c("AC1","AC2","AC3")
  Retirement_Date<-c("11/27/2021","9/20/2020","12/5/2050")
  dfAircraft_CY<-data.frame(Aircraft_Number, Retirement_Date)
  
  Aircraft_Number <- c("AC1","AC2","AC4")
  Retirement_Date<-c("11/27/2026","9/20/2023","12/5/2050")
  dfAircraft_LY<-data.frame(Aircraft_Number, Retirement_Date)
  
  Engine_Number<-c("Eng1","Eng2","Eng3")
  ECSV<-c("6453","3000","12000")
  EventType<-c("0","SV","QT")
  dfEngine_CY<-data.frame(Engine_Number,ECSV,EventType)
  
  Engine_Number<-c("Eng1","Eng2","Eng3")
  ECSV<-c("3400","200","11000")
  EventType<-c("0","0","0")
  dfEngine_LY<-data.frame(Engine_Number,ECSV,EventType)
  
  
  return(importedDFList<<-list("Aircraft_CY"=dfAircraft_CY,
                               "Aircraft_LY"=dfAircraft_LY,
                               "Engine_CY"=dfEngine_CY,
                               "Engine_LY"=dfEngine_LY
  ))
}

#data formatting
formatImportedData<-function(dfAircraft_CY=importedDFList$Aircraft_CY,
                             dfAircraft_LY=importedDFList$Aircraft_LY,
                             dfEngine_CY=importedDFList$Engine_CY,
                             dfEngine_LY=importedDFList$Engine_LY){
  
  dfAircraft_CY$Aircraft_Number<-as.character(dfAircraft_CY$Aircraft_Number)
  dfAircraft_CY$Retirement_Date<-as.character(dfAircraft_CY$Retirement_Date)
  
  dfAircraft_LY$Aircraft_Number<-as.character(dfAircraft_LY$Aircraft_Number)
  dfAircraft_LY$Retirement_Date<-as.character(dfAircraft_LY$Retirement_Date)
  
  dfEngine_CY$Engine_Number<-as.character(dfEngine_CY$Engine_Number)
  dfEngine_CY$ECSV<-as.character(dfEngine_CY$ECSV)
  dfEngine_CY$EventType<-as.character(dfEngine_CY$EventType)
  
  dfEngine_LY$Engine_Number<-as.character(dfEngine_LY$Engine_Number)
  dfEngine_LY$ECSV<-as.character(dfEngine_LY$ECSV)
  dfEngine_LY$EventType<-as.character(dfEngine_LY$EventType)
  
  return(formattedDFList<<-list("Aircraft_CY"=dfAircraft_CY,
                                "Aircraft_LY"=dfAircraft_LY,
                                "Engine_CY"=dfEngine_CY,
                                "Engine_LY"=dfEngine_LY))
}

#data processing
getEngineDF<-function(dfEngine_CY=formattedDFList$Engine_CY,
                        dfEngine_LY=formattedDFList$Engine_LY){
  contractEndDate<-"03/31/2035"
  cutoffDate<-"08/01/2021"
  
  dfEngineJoined=left_join(dfEngine_CY, dfEngine_LY, by=c("Engine_Number"), suffix=c("_CY","_LY"))
  
  return(validateEngine(dfEngineJoined))
}
getAircraftDF<-function(dfAircraft_CY=formattedDFList$Aircraft_CY,
                        dfAircraft_LY=formattedDFList$Aircraft_LY){
  contractEndDate<-"03/31/2035"
  cutoffDate<-"08/01/2021"
  
  dfAircraftJoined=left_join(dfAircraft_CY, dfAircraft_LY, by=c("Aircraft_Number"), suffix=c("_CY","_LY"))
  dfAircraftJoined[,"contractEndDate"]<-contractEndDate
  dfAircraftJoined[,"cutoffDate"]<-cutoffDate
  
  return(validateAircraft(dfAircraftJoined))
}

#calculate all validation tests
validateAircraft<-function(dfAircraft){
  dfAircraft<-dfAircraft%>%
    mutate(`1.ValidationTest (Date_CY=Date_LY)`=if_else(Retirement_Date_CY==Retirement_Date_LY,TRUE,FALSE, missing = FALSE))%>%
    mutate(`2.ValidationTest (Date_CY < contractEndDate)`=as.Date(Retirement_Date_CY, format="%m/%d/%Y") < as.Date(contractEndDate, format="%m/%d/%Y"))
  joinedDFList$Aircraft<<-dfAircraft
  
  return(dfAircraft)
}
validateEngine<-function(dfEngine){
  
  dfEngine<-dfEngine%>%
    mutate(`3.ValidationTest (ECSV reset check)`=case_when(
      EventType_CY=="0" & as.numeric(ECSV_CY)>=as.numeric(ECSV_LY)~"OK, no reset",
      EventType_CY=="0" & as.numeric(ECSV_CY)<as.numeric(ECSV_LY)~"Not OK, ECSV should be reset but there was no SV",
      EventType_CY!="0" & as.numeric(ECSV_CY)>=as.numeric(ECSV_LY)~"Not OK, ECSV should be reset but isn't",
      EventType_CY!="0" & as.numeric(ECSV_CY)<as.numeric(ECSV_LY)~"OK, ECSV reset",
      TRUE ~ "NA"
    ))
  joinedDFList$Engine<<-dfEngine
  return(dfEngine)
}

#function which subset certain valid test based on desired output and test number
ValidationTest<-function(option, testnumber, dfAircraftJoined=joinedDFList$Aircraft, dfEngineJoined=joinedDFList$Engine){
  result=switch(
    testnumber,
    "1"=ValidationTest1(option, df=dfAircraftJoined),
    "2"=ValidationTest2(option, df=dfAircraftJoined),
    "3"=ValidationTest3(option, df=dfEngineJoined)
  )
  return(result)
}

#accessory function needed for audit generation
rowToString<-function(df,col){
  i<-1
  my_string<-""
  if (nrow(df)==0){
    return("None")
  } else {
    for (i in 1:nrow(df)){
      my_string<-paste(my_string, df[i,col], sep=", ")
    }
    return(sub(", ", "", my_string))
  }
}

#validation tests
ValidationTest1<-function(option, df=dfAircraftJoined){
  df<-df%>%
    filter(`1.ValidationTest (Date_CY=Date_LY)`==FALSE)%>%
    select(Aircraft_Number, Retirement_Date_CY, Retirement_Date_LY, `1.ValidationTest (Date_CY=Date_LY)`)
  if (option=="comment"){
    return(paste("Number of non-matchnig retirement dates: ", nrow(df)," (", rowToString(df, "Aircraft_Number"), ")"))
  } else if (option=="df"){
    return(df)
  } else if (option=="worksheet"){
    return("Aircraft")
  } else if (option=="info"){
    return("Retirement dates Y-O-Y")
  }
}
ValidationTest2<-function(option, df=dfAircraftJoined){
  df<-df%>%
    filter(`2.ValidationTest (Date_CY < contractEndDate)`==FALSE)%>%
    select(Aircraft_Number, Retirement_Date_CY, contractEndDate, `2.ValidationTest (Date_CY < contractEndDate)`)
  if (option=="comment"){
    return(paste("Number of aircrafts where retirement date > contract end date: ", nrow(df)," (", rowToString(df, "Aircraft_Number"), ")"))
  } else if (option=="df"){
    return(df)
  } else if (option=="worksheet"){
    return("Aircraft")
  } else if (option=="info"){
    return("Retirement date < contract end date")
  }
}
ValidationTest3<-function(option, df=dfEngineJoined){
  df<-df%>%
    filter(startsWith(`3.ValidationTest (ECSV reset check)`, "Not OK"))%>%
    select(Engine_Number, ECSV_CY, ECSV_LY, EventType_CY, `3.ValidationTest (ECSV reset check)`)
  if (option=="comment"){
    return(paste("Number of engines where ECSV reset check is not ok: ", nrow(df)," (", rowToString(df, "Engine_Number"), ")"))
  } else if (option=="df"){
    return(df)
  } else if (option=="worksheet"){
    return("Engine")
  } else if (option=="info"){
    return("ECSV reset check")
  }
}

# Here you can add which column should be able to be filtered as categorial data
# columnToFactors<-function(){
#   d1$`EventType_CY`<<-as.factor(d1$`EventType_CY`)
# }

#function generates colname vectors for valid tests
generateColnamesFilter<-function(dfAircraft=joinedDFList$Aircraft, dfEngine=joinedDFList$Engine){
  colnamesDF<-data.frame(`Test #`=as.character())
  colnames(colnamesDF)<-c("Test #")
  colnamesList<-list()
  i<-1
  testVector<-c("1","2","3")
  for (testNumber in testVector){
    print(testNumber)
    colnamesDF[i,1]<-testNumber
    colnamesList[[testNumber]]<-colnames(ValidationTest("df", testNumber, dfAircraftJoined = dfAircraft, dfEngineJoined = dfEngine))
    i<-i+1
  }
  colnamesList<<-colnamesList
  return(colnamesList)
}

#filter parameter generation based on test number and dataset
rowFilter<-function(testNumber,df){
  result=switch(
    testNumber,
    "1"=df$`1.ValidationTest (Date_CY=Date_LY)`==FALSE,
    "2"=df$`2.ValidationTest (Date_CY < contractEndDate)`==FALSE,
    "3"=startsWith(as.character(df$`3.ValidationTest (ECSV reset check)`), "Not OK"),
    "all"=1:nrow(df)
  )
}

#audit generation
GenerateAudit<-function(dfAircraft=joinedDFList[["Aircraft"]],
                        dfEngine=joinedDFList[["Engine"]]){
  
  validDataFrame<-data.frame(`Test #`=as.character(), `DataFrame`=as.character(), `Details`=as.character(), `Comment`=as.character())
  colnames(validDataFrame)<-c("Test #", "Worksheet", "Details", "Comment")
  validDataProves<-list("Aircraft"=dfAircraft, "Engine"=dfEngine, "Audit"=data.frame())
  i<-1
  testVector<-c("1","2","3")
  for (testNumber in testVector){
    print(testNumber)
    validDataFrame[i,1]<-testNumber
    validDataFrame[i,2]<-ValidationTest("worksheet",testNumber, dfAircraftJoined = dfAircraft, dfEngineJoined = dfEngine)
    validDataFrame[i,3]<-ValidationTest("info",testNumber, dfAircraftJoined = dfAircraft, dfEngineJoined = dfEngine)
    validDataFrame[i,4]<-ValidationTest("comment",testNumber, dfAircraftJoined = dfAircraft, dfEngineJoined = dfEngine)
    validDataProves[[testNumber]]<-ValidationTest("df",testNumber, dfAircraftJoined = dfAircraft, dfEngineJoined = dfEngine)
    i<-i+1
  }
  validDataProves[["Audit"]]<-validDataFrame
  return(validDataProves)
}