# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyBS")
# install.packages("tidyverse")
# install.packages("DT")
# install.packages("readxl")
# install.packages("writexl")
# install.packages("shinybusy")
# install.packages("naniar")

library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(DT)
library(readxl)
library(writexl)
library(shinybusy)
library(naniar)

#Load functions needed to use the app
source(paste(getwd(), "Functions.R",sep = "/"))

#Specify which colnames ale the validation test ones, needed to formatstyle function
validColnames<-c("1.ValidationTest (Date_CY=Date_LY)", "2.ValidationTest (Date_CY < contractEndDate)", "3.ValidationTest (ECSV reset check)")

#Initialize epty df for edited records
if (exists("d1")){
    editedRowsAircraft<<-d1[0,]
}
if (exists("d2")){
    editedRowsEngine<<-d2[0,]
}
joinedDFList<<-list("Aircraft"=data.frame(),"Engine"=data.frame())
# UI definition
ui <- fluidPage(
    
    #Loading animation
    add_busy_spinner(spin="fading-circle"),
    tags$head(tags$style(
        type='text/css',
        '.has-feedback .form-control { padding-right: 0px; }
          form.well { max-height: 475px; overflow-y: auto; }'
    )),
    
    titlePanel("Validation Test App"),
    tabsetPanel(
        tabPanel("ImportData",
                 sidebarLayout(
                     sidebarPanel(width = 4,
                                  actionButton('btnImport', 'Import Data'),
                                  actionButton('btnOldData', 'Load existing data'),
                                  hr(),
                                  ),
                     mainPanel(
                         verbatimTextOutput("manual")                     )
                 )
        ),
        tabPanel("Aircraft",
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  actionButton("validateAircraft", "Revalidate table"),
                                  actionButton("saveBtnAircraft", "Save data to buffer"),
                                  actionButton("saveToExcelAircraft", "Buffer data to Excel"),
                                  actionButton("exitBtnAircraft","Exit App"),
                                  hr(),
                                  selectInput("dataset2", "Show desired tables", choices = c("Aircraft","1.ValidationTest (Date_CY=Date_LY)","2.ValidationTest (Date_CY < contractEndDate)")),
                                  selectInput("colnames", "Select desired columns", choices = NULL, multiple = TRUE)),
                     mainPanel(
                         DT::dataTableOutput("tableAircraft"), style="height:100%; width:75%; overflow-y: scroll; overflow-x: scroll; font-size:75%;"
                     ), 
                     position = "left", fluid = TRUE)
        ),
        tabPanel("Engine",
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  actionButton("validateEngine", "Revalidate table"),
                                  actionButton("saveBtnEngine", "Save data to buffer"),
                                  actionButton("saveToExcelEngine", "Buffer data to Excel"),
                                  actionButton("exitBtnEngine","Exit App"),
                                  hr(),
                                  selectInput("datasetEngine", "Show desired tables", choices = c("Engine","3.ValidationTest (ECSV reset check)")),
                                  selectInput("colnamesEngine", "Select desired columns", choices = NULL, multiple = TRUE)),
                     mainPanel(
                         DT::dataTableOutput("tableEngine"), style="height:100%; width:75%; overflow-y: scroll; overflow-x: scroll; font-size:75%;"
                     ), 
                     position = "left", fluid = TRUE)
        ),
        tabPanel("Audit",
                 fluidRow(
                     actionButton("regenerateAudit", "regenerate Audit"),
                     dataTableOutput("tableAudit")
                 )
        )
    )
)

# Server logic definition
server <- function(input, output, session) {
    
    #import data button
    observeEvent(input$btnImport,{
        show_modal_spinner(
            spin="semipolar",
            color="#0066FF",
            text=NULL,
            session=shiny::getDefaultReactiveDomain()
        )
        #initialize empty list

        
        #Funtion imports data
        importData()
        
        #Format imported data if there is one
        observe({
            if (!is.null(importedDFList)){
                formatImportedData()
                cat("FOrmattedDFlist completed\n")
            } else {
                cat("There is no importedDFList\n")
            }
            if (!is.null(formattedDFList)){
                
                #Data processing on Aircraft dataframe
                getAircraftDF()
                #Data processing on Engine dataframe
                getEngineDF()
                
                #Creating global variables, d1/d2 contain edited records, d1/d2_initial are the reference ones
                d1<<-joinedDFList$Aircraft
                d2<<-joinedDFList$Engine
                d1_Initial<<-joinedDFList$Aircraft
                d2_Initial<<-joinedDFList$Engine
                #columnToFactors()
                save(joinedDFList, file = "joinedDFList.Rda")
                colnamesList<<-generateColnamesFilter()
                save(colnamesList, file = "colnamesList.Rda")
                excelList<<-list()
                i<<-1
                j<<-1
                k<<-1
                engineSetToSave<<-vector()
                aircraftSetToSave<<-vector()
                
                remove_modal_spinner(
                    session=getDefaultReactiveDomain()
                )
                refreshTable()
            }
        })
    })
    
    #waits for any changes inside tables
    refreshTable<-reactive({
        observe({
            if (exists("joinedDFList")){
                
                #tables rendering
                output$tableAircraft <- renderDataTable({
                    k<-1
                    h<-0
                    for (i in validColnames){
                        ifelse(i %in% colnames(d1[,input$colnames]), h[k]<-i, next)
                        k<-k+1
                    }
                    df<-datatable(
                        d1[xx, input$colnames],
                        editable = TRUE,
                        rownames = TRUE,
                        filter = "top",
                        extensions = c("Buttons", "Scroller"),
                        options = list(
                            searching = TRUE,
                            autoWidth = FALSE,
                            ordering = TRUE,
                            dom = "Bftrip",
                            autFill = TRUE,
                            scrollY = TRUE,
                            scrollX = TRUE,
                            pageLength = 1000,
                            deferRender = TRUE
                        ),
                        class = "cell-border stripe"
                    ) %>%
                        formatStyle(h, target="cell", backgroundColor = styleEqual(c(1,0), c("lightgreen", "lightpink")))
                    return (df)
                }, server = FALSE)
                output$tableEngine <- renderDataTable({
                    k<-1
                    h<-0
                    for (i in validColnames){
                        ifelse(i %in% colnames(d2[,input$colnamesEngine]), h[k]<-i, next)
                        k<-k+1
                    }
                    df<-datatable(
                        d2[xx, input$colnamesEngine],
                        editable = TRUE,
                        rownames = TRUE,
                        filter = "top",
                        extensions = c("Buttons", "Scroller"),
                        options = list(
                            searching = TRUE,
                            autoWidth = FALSE,
                            ordering = TRUE,
                            dom = "Bftrip",
                            autFill = TRUE,
                            scrollY = TRUE,
                            scrollX = TRUE,
                            pageLength = 1000,
                            deferRender = TRUE
                        ),
                        class = "cell-border stripe"
                    ) %>%
                        formatStyle(h, target="cell", backgroundColor = styleEqual(c(1,0), c("lightgreen", "lightpink")))
                    return (df)
                }, server = FALSE)
                
                #filtering parameters
                observe({
                    if(input$dataset2=="1.ValidationTest (Date_CY=Date_LY)"){
                        x<- as.vector(colnamesList[["1"]])
                        xx<<-rowFilter("1",d1)
                    } else if(input$dataset2=="2.ValidationTest (Date_CY < contractEndDate)"){
                        x<-as.vector(colnamesList[["2"]])
                        xx<<-rowFilter("2",d1)
                    } else {
                        x<-as.vector(colnames(d1))
                        xx<<-rowFilter("all",d1)
                    }
                    updateSelectInput(session, inputId = "colnames", label = "colnames", choices = colnames(joinedDFList[["Aircraft"]]), selected = x)
                })
                observe({
                    if(input$datasetEngine=="3.ValidationTest (ECSV reset check)"){
                        x<- as.vector(colnamesList[["3"]])
                        xx<<-rowFilter("3",d2)
                    } else {
                        x<-as.vector(colnames(d2))
                        xx<<-rowFilter("all",d2)
                    }
                    updateSelectInput(session, inputId = "colnamesEngine", label = "colnames", choices = colnames(joinedDFList[["Engine"]]), selected = x)
                })
                
                #exit app button
                observeEvent(input$exitBtnAircraft,{
                    updateSelectInput(session, inputId = "colnames", label = "colnames", choices = colnames(joinedDFList[["Aircraft"]]), selected=colnames(joinedDFList[["Aircraft"]]))
                    updateSelectInput(session, inputId = "colnamesEngine", label = "colnames", choices = colnames(joinedDFList[["Engine"]]), selected=colnames(joinedDFList[["Engine"]]))
                    joinedDFList[["Aircraft"]]<<- d1
                    joinedDFList[["Engine"]]<<- d2
                    save(joinedDFList, file = "joinedDFList.Rda")
                    cat("exit data saved")
                    stopApp()
                    })
                observeEvent(input$exitBtnEngine,{
                    updateSelectInput(session, inputId = "colnames", label = "colnames", choices = colnames(joinedDFList[["Aircraft"]]), selected=colnames(joinedDFList[["Aircraft"]]))
                    updateSelectInput(session, inputId = "colnamesEngine", label = "colnames", choices = colnames(joinedDFList[["Engine"]]), selected=colnames(joinedDFList[["Engine"]]))
                    joinedDFList[["Aircraft"]]<<- d1
                    joinedDFList[["Engine"]]<<- d2
                    save(joinedDFList, file = "joinedDFList.Rda")
                    cat("exit data saved")
                    stopApp()
                })
                
                #save to buffer button
                observeEvent(input$saveBtnAircraft,{
                    aircraftSetToSave[i]<<-as.character(input$dataset2)
                    i<<-i+1
                    print(paste(as.character(input$dataset2), "data prepared to excel save", sep=" "))
                    save(joinedDFList, file = "joinedDFList.Rda")
                    print("Aircraft data saved to joinedDFList")
                })
                observeEvent(input$saveBtnEngine,{
                    engineSetToSave[i]<<-as.character(input$datasetEngine)
                    i<<-i+1
                    print(paste(as.character(input$datasetEngine), "data prepared to excel save", sep=" "))
                    save(joinedDFList, file = "joinedDFList.Rda")
                    print("Engine data saved to joinedDFList")
                })
                
                #save buffer to excel button
                observeEvent(input$saveToExcelAircraft,{
                    
                    d1<<-validateAircraft(d1)
                    d2<<-validateEngine(d2)

                    d1WithChangedRowsAdded<-full_join(mutate_all(d1_Initial[1:5], as.character), setdiff(mutate_all(d1[1:5],as.character), mutate_all(d1_Initial[1:5], as.character)), by="Aircraft_Number", keep = TRUE, suffix=c("","_Corrected"))
                    d2WithChangedRowsAdded<-full_join(mutate_all(d2_Initial[1:5], as.character), setdiff(mutate_all(d2[1:5],as.character), mutate_all(d2_Initial[1:5], as.character)), by="Engine_Number", keep = TRUE, suffix=c("","_Corrected"))
                    
                    excelList[["Aircraft"]]<-inner_join(mutate_all(d1_Initial, as.character), d1WithChangedRowsAdded, by="Aircraft_Number" )
                    excelList[["Engine"]]<-inner_join(mutate_all(d2_Initial, as.character), d2WithChangedRowsAdded, by="Engine_Number" )
                    
                    for (ii in unique(aircraftSetToSave)){
                        changedDF<-d1[,as.vector(colnamesList[[strsplit(ii, "[.]")[[1]][1]]])]
                        initialDF<-d1_Initial[,as.vector(colnamesList[[strsplit(ii, "[.]")[[1]][1]]])]
                        changedRows<-setdiff(changedDF,initialDF)
                        changedJoinedWithInitial<-full_join(mutate_all(initialDF, as.character), mutate_all(changedRows, as.character), by=c("Aircraft_Number"), suffix=c("","_Corrected"))
                        excelList[[ii]]<-changedJoinedWithInitial
                    }
                    
                    for (ii in unique(engineSetToSave)){
                        changedDF<-d2[,as.vector(colnamesList[[strsplit(ii, "[.]")[[1]][1]]])]
                        initialDF<-d2_Initial[,as.vector(colnamesList[[strsplit(ii, "[.]")[[1]][1]]])]
                        changedRows<-setdiff(changedDF,initialDF)
                        changedJoinedWithInitial<-full_join(mutate_all(initialDF, as.character), mutate_all(changedRows, as.character), by=c("Engine_Number"), suffix=c("","_Corrected"))
                        excelList[[ii]]<-changedJoinedWithInitial
                    }
                    cat("data preapred to save")
                    write_xlsx(excelList, path=paste(getwd(),"excelList.xlsx",sep="/"))
                    save(joinedDFList, file="joinedDFList.Rda")
                    cat("data saved to JointedDFlist")
                })
                observeEvent(input$saveToExcelEngine,{
                    
                    d1<<-validateAircraft(d1)
                    d2<<-validateEngine(d2)
                    
                    d1WithChangedRowsAdded<-full_join(mutate_all(d1_Initial[1:5], as.character), setdiff(mutate_all(d1[1:5],as.character), mutate_all(d1_Initial[1:5], as.character)), by="Aircraft_Number", keep = TRUE, suffix=c("","_Corrected"))
                    d2WithChangedRowsAdded<-full_join(mutate_all(d2_Initial[1:5], as.character), setdiff(mutate_all(d2[1:5],as.character), mutate_all(d2_Initial[1:5], as.character)), by="Engine_Number", keep = TRUE, suffix=c("","_Corrected"))
                    
                    excelList[["Aircraft"]]<-inner_join(mutate_all(d1_Initial, as.character), d1WithChangedRowsAdded, by="Aircraft_Number" )
                    excelList[["Engine"]]<-inner_join(mutate_all(d2_Initial, as.character), d2WithChangedRowsAdded, by="Engine_Number" )
                    
                    for (ii in unique(aircraftSetToSave)){
                        changedDF<-d1[,as.vector(colnamesList[[strsplit(ii, "[.]")[[1]][1]]])]
                        initialDF<-d1_Initial[,as.vector(colnamesList[[strsplit(ii, "[.]")[[1]][1]]])]
                        changedRows<-setdiff(changedDF,initialDF)
                        changedJoinedWithInitial<-full_join(mutate_all(initialDF, as.character), mutate_all(changedRows, as.character), by=c("Aircraft_Number"), suffix=c("","_Corrected"))
                        excelList[[ii]]<-changedJoinedWithInitial
                    }
                    
                    for (ii in unique(engineSetToSave)){
                        changedDF<-d2[,as.vector(colnamesList[[strsplit(ii, "[.]")[[1]][1]]])]
                        initialDF<-d2_Initial[,as.vector(colnamesList[[strsplit(ii, "[.]")[[1]][1]]])]
                        changedRows<-setdiff(changedDF,initialDF)
                        changedJoinedWithInitial<-full_join(mutate_all(initialDF, as.character), mutate_all(changedRows, as.character), by=c("Engine_Number"), suffix=c("","_Corrected"))
                        excelList[[ii]]<-changedJoinedWithInitial
                    }
                    cat("data preapred to save")
                    write_xlsx(excelList, path=paste(getwd(),"excelList.xlsx",sep="/"))
                    save(joinedDFList, file="joinedDFList.Rda")
                    cat("data saved to JointedDFlist")
                })
            }
        })
    })
    
    #load existing data button
    observeEvent(input$btnOldData,{
        
        cat("loading previosuly generated data\n")
        
        load(file = paste(getwd(), "JoinedDFList.Rda",sep = "/"), envir = .GlobalEnv)
        #load(file = paste(getwd(), "ValidDataProves.Rda",sep = "/"), envir = .GlobalEnv)
        
        d1<<-joinedDFList[["Aircraft"]]
        d2<<-joinedDFList[["Engine"]]
        
        excelList<<-list()
        i<<-1
        j<<-1
        k<<-1
        engineSetToSave<<-vector()
        aircraftSetToSave<<-vector()
        
        # columnToFactors()
        
        cat("previously generated data loaded\n")
        
        refreshTable()
        
    })
    
    #editabe table cells
    observeEvent(input$tableAircraft_cell_edit,{
        d1[xx,input$colnames][input$tableAircraft_cell_edit$row,input$tableAircraft_cell_edit$col]<<- as.character(input$tableAircraft_cell_edit$value)
    })
    observeEvent(input$tableEngine_cell_edit,{
        d2[xx,input$colnamesEngine][input$tableEngine_cell_edit$row,input$tableEngine_cell_edit$col]<<- as.character(input$tableEngine_cell_edit$value)
    })
    
    #revalidation button
    observeEvent(input$validateAircraft, {
        show_modal_spinner()
        d1<<-validateAircraft(d1)
        #columnToFactors()
        remove_modal_spinner()
        print("Aircraft table revalidated")
        output$tableAircraft<-renderDataTable({
            k<-1
            h<-0
            for (i in validColnames){
                ifelse(i %in% colnames(d1[,input$colnames]), h[k]<-i, next)
                k<-k+1
            }
            df<-datatable(
                d1[xx, input$colnames],
                editable = TRUE,
                rownames = TRUE,
                filter = "top",
                extensions = c("Buttons", "Scroller"),
                options = list(
                    searching = TRUE,
                    autoWidth = FALSE,
                    ordering = TRUE,
                    dom = "Bftrip",
                    autFill = TRUE,
                    scrollY = "600px",
                    scrollX = TRUE,
                    pageLength = 1000,
                    deferRender = TRUE
                ),
                class = "cell-border stripe"
            ) %>%
                formatStyle(h, target="cell", backgroundColor = styleEqual(c(1,0), c("lightgreen", "lightpink")))
            return (df)
        }, server = FALSE)
        })
    observeEvent(input$validateEngine, {
        show_modal_spinner()
        d2<<-validateEngine(d2)
        #columnToFactors()
        remove_modal_spinner()
        print("Aircraft table revalidated")
        output$tableEngine<-renderDataTable({
            k<-1
            h<-0
            for (i in validColnames){
                ifelse(i %in% colnames(d2[,input$colnamesEngine]), h[k]<-i, next)
                k<-k+1
            }
            df<-datatable(
                d2[xx, input$colnamesEngine],
                editable = TRUE,
                rownames = TRUE,
                filter = "top",
                extensions = c("Buttons", "Scroller"),
                options = list(
                    searching = TRUE,
                    autoWidth = FALSE,
                    ordering = TRUE,
                    dom = "Bftrip",
                    autFill = TRUE,
                    scrollY = "600px",
                    scrollX = TRUE,
                    pageLength = 1000,
                    deferRender = TRUE
                ),
                class = "cell-border stripe"
            ) %>%
                formatStyle(h, target="cell", backgroundColor = styleEqual(c(1,0), c("lightgreen", "lightpink")))
            return (df)
        }, server = FALSE)
        
    })
    
    #audit generation button
    observeEvent(input$regenerateAudit,{
        show_modal_spinner(
            spin="semipolar",
            color="#0066FF",
            text=NULL,
            session=shiny::getDefaultReactiveDomain()
        )
        tempdf<-GenerateAudit(dfAircraft = d1, dfEngine = d2)$Audit
        remove_modal_spinner(
            session = getDefaultReactiveDomain()
        )
        
        output$tableAudit<-renderDataTable({
            df<-datatable(
                tempdf,
                rownames = TRUE,
                filter = "top",
                extensions = c("Buttons", "Scroller"),
                options = list(
                    searching = TRUE,
                    autoWidth = FALSE,
                    ordering = TRUE,
                    dom = "Bftrip",
                    autFill = TRUE,
                    scrollY = "600px",
                    scrollX = TRUE,
                    pageLength = 1000,
                    deferRender = TRUE
                ),
                class = "cell-border stripe"
            )
        })
    })
    
    #user manual output
    output$manual<-renderText({
        "Import Data - generuje oraz waliduje dane testowe
        \nLoad existing Data - wczytuje dane aktualne na stan poprzedniego zamkniecia aplikacji przyciskiem Exit App
        \nZakladki Aircraft/Engine - zbiory przetworzonych danych wraz z dodanymi testami walidacyjnymi
        \nZakladka Audit - generowanie raportu zliczajacego wyniki testow walidacyjnych
        \nFunkcjonalnosci: 
        \n-rewalidacja edytowalnej tabeli;
        \n-ukrywanie/pokazywanie kolumn;
        \n-wybieranie poszczegolnych testow walidacyjnych z narzuconym filtrowaniem testow negatywnych;
        \n-zapis do bufora poszczegolnych testow, zawierajacych zestawienie danych przed edycja oraz po edycji;
        \n-odpis z bufora do pliku excel
        "
    })
}

# Run the application 
shinyApp(ui=ui, server=server)
