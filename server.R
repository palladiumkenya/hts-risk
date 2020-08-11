# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    predictors <- eventReactive(input$pred, {
        
        validate(need(input$ageattest, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$KPtype, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$maritalstatus, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$gender, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$patientdisabled, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$evertested, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$monthssincelasttest, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$clienttestedas, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$entrypoint, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$testingstrategy, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$tbscreening, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$clientselftested, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$couplediscordant, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$sitecode, "Cannot generate prediction: missing at least one input value"))
        
        df <- data.frame(AgeAtTest = as.numeric(input$ageattest),
                   KeyPopulationType = factor(input$KPtype, levels = levels(dat$KeyPopulationType)),
                   MaritalStatus = factor(input$maritalstatus, levels = levels(dat$MaritalStatus)),
                   Gender = factor(input$gender, levels = levels(dat$Gender)),
                   PatientDisabled = factor(input$patientdisabled, levels = levels(dat$PatientDisabled)),
                   EverTestedForHIV = factor(input$evertested, levels = levels(dat$EverTestedForHIV)),
                   MonthsSinceLastTest = as.numeric(input$monthssincelasttest),
                   ClientTestedAs = factor(input$clienttestedas, levels = levels(dat$ClientTestedAs)),
                   EntryPoint = factor(input$entrypoint, levels = levels(dat$EntryPoint)),
                   TestingStrategy = factor(input$testingstrategy, levels = levels(dat$TestingStrategy)),
                   TBScreening = factor(input$tbscreening, levels = levels(dat$TBScreening)),
                   ClientSelfTested = factor(input$clientselftested, levels = levels(dat$ClientSelfTested)),
                   CoupleDiscordant = factor(input$couplediscordant, levels = levels(dat$CoupleDiscordant)),
                   Sitecode = factor(input$sitecode, levels = levels(dat$Sitecode)))
        
        
        showModal(modalDialog(
            title = "Prediction Generated"
        ))

        df
        
    })
    
    prediction <- reactive({
        
        predict(mod, newdata=predictors(), type = "prob")
        
    })    
    
    output$predText <- renderText({
        
        if(prediction()[, 2] > THRESH_75[3]){
            sprintf("Highest Risk \n %s of patients with this risk score or higher tested positive. 
                    These patients account for %s of all positive test results",
                    THRESH_75[2], THRESH_75[1])
        } else if(prediction()[, 2] > THRESH_50[3]){
            sprintf("High Risk \n %s of patients with this risk score or higher tested positive. 
                    These patients account for %s of all positive test results",
                    THRESH_50[2], THRESH_50[1])
        } else if(prediction()[, 2] > THRESH_25[3]){
            sprintf("Medium Risk \n %s of patients with this risk score or higher tested positive. 
                    These patients account for %s of all positive test results",
                    THRESH_25[2], THRESH_25[1])
        } else {"Low Risk"}

    })
    
    observeEvent(input$recPred, {
        
        showModal(modalDialog(
            title = "Record Prediction",
            br(),
            "Please enter username and password to record prediction",
            br(),
            textInput("usrnm", "Username", value = ""),
            textInput("pswrd", "Password", value = ""),
            actionButton("submitPred", "Submit")
            # actionButton("recPredFinal", "Yes, Record Prediction")
        ))
 
    })
    
    observeEvent(input$submitPred, {
        
        conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        user_auth <- dbGetQuery(conn, "SELECT * FROM NairobiAccess WHERE usernames = ? AND passwords = ?", 
                                params = c(input$usrnm, input$pswrd))
        dbDisconnect(conn)
        
        # validate(need(nrow(user_auth) == 1, "Sorry, Username and Password not recognized. Please try again."))

        if(nrow(user_auth) == 0){
            
            showModal(modalDialog(
                title = "User Not Recognized",
                br(), br(),
                "Sorry, Username and Password not recognized. Please try again."))
        }
        
        if(nrow(user_auth) == 1){
            
        # Get vector of IDs    
            
        showModal(modalDialog(
            title = "User Acknowledged",
            br(), br(),
            "Username and Password Acknowledged",
            br(), br(),
            actionButton("recPredFinal", "Record Prediction")
        ))
        }
    })
    
    observeEvent(input$recPredFinal, {
        
        conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        df <- data.frame(ID = NA, predictors(), Prediction = prediction()[, 2], TestResult = 'Pending', TimeofTest = 'Pending')
        dbWriteTable(conn, "NairobiHTS", df, append = TRUE)
        id_new <- dbGetQuery(conn, "SELECT MAX(ID) FROM NairobiHTS")
        dbDisconnect(conn)

        showModal(modalDialog(paste("Record ID for this test is", id_new)))
        
    })
    
    observeEvent(input$recResult, {
        
        showModal(modalDialog(
            title = "Record Test Result",
            br(),
            "Please enter username and password to record test result",
            br(),
            textInput("usrnm_res", "Username", value = ""),
            textInput("pswrd_res", "Password", value = ""),
            actionButton("submitResult", "Submit")
        ))
    })
    
    observeEvent(input$submitResult, {
        
        conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        user_auth <- dbGetQuery(conn, "SELECT * FROM NairobiAccess WHERE usernames = ? AND passwords = ?", 
                                params = c(input$usrnm_res, input$pswrd_res))
        dbDisconnect(conn)
        
        if(nrow(user_auth) == 0){
            
            showModal(modalDialog(
                title = "User Not Recognized",
                br(), br(),
                "Sorry, Username and Password not recognized. Please try again."))
        }
        
        if(nrow(user_auth) == 1){
    
        conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        ids <- dbGetQuery(conn, "SELECT ID FROM NairobiHTS")
        dbDisconnect(conn)
        
        showModal(modalDialog(
            title = "Record Test Result",
            br(),
            numericInput("id_input", "Select ID", value = max(ids), min = 1, max = max(ids)),
            selectInput('testResult', "Input Test Result", choices = c('', 'Positive', 'Negative', 'Inconclusive')),
            actionButton('recResultFinal', 'Record Test Result')
        ))
        }
    })
    
    observeEvent(input$recResultFinal, {
        
        conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        dbExecute(conn, "UPDATE NairobiHTS SET TestResult = ? where ID = ?", params = c(input$testResult, input$id_input))
        dbExecute(conn, "UPDATE NairobiHTS SET TimeofTest = ? where ID = ?", params = c(input$testResult, as.character(Sys.time())))
        dbDisconnect(conn)
        
    })


})
