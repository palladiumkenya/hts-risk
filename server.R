# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    predictors <- eventReactive(input$pred, {
        
        validate(need(input$HTSNumber, "Missing HTS Number"))
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
        # validate(need(input$sitecode, "Cannot generate prediction: missing at least one input value"))
    
        # Get facility information
        facility <- facilities[facilities$Facility.Name == input$facilityname, ]
        facility_df <- facility %>% select(-Facility.Name)

        # For KP and Testing Strategy, convert to other for non-common values
        kp <- ifelse(input$KPtype %in% c("MSM"), "OtherKP", input$KPtype)
        ms <- ifelse(input$maritalstatus == "Single", NA, input$maritalstatus)
        ts <- ifelse(input$testingstrategy %in% c("NP", "HP", "PNS"), "Other", input$testingstrategy)

        df <- data.frame(AgeAtTest = as.numeric(input$ageattest),
                   KeyPopulationType = factor(kp, levels = levels(dat$KeyPopulationType)),
                   MaritalStatus = factor(ms, levels = levels(dat$MaritalStatus)),
                   Gender = factor(input$gender, levels = levels(dat$Gender)),
                   PatientDisabled = factor(input$patientdisabled, levels = levels(dat$PatientDisabled)),
                   EverTestedForHIV = factor(input$evertested, levels = levels(dat$EverTestedForHIV)),
                   MonthsSinceLastTest = as.numeric(input$monthssincelasttest),
                   ClientTestedAs = factor(input$clienttestedas, levels = levels(dat$ClientTestedAs)),
                   EntryPoint = factor(input$entrypoint, levels = levels(dat$EntryPoint)),
                   TestingStrategy = factor(ts, levels = levels(dat$TestingStrategy)),
                   TBScreening = factor(input$tbscreening, levels = levels(dat$TBScreening)),
                   ClientSelfTested = factor(input$clientselftested, levels = levels(dat$ClientSelfTested)),
                   month_of_test = factor(month(Sys.time()), levels = levels(dat$month_of_test)),
                   # Sitecode = factor(input$sitecode, levels = levels(dat$Sitecode)),
                   dayofweek = factor(wday(Sys.time()), levels = levels(dat$dayofweek)))

        df <- cbind(df, facility_df) 

        df <- encodeXGBoost(df) %>%
            select(mod$feature_names)


        showModal(modalDialog(
            title = "Prediction Generated"
        ))

        df
        
    })
    
    prediction <- reactive({
        
        predict(mod, data.matrix(predictors()))
        
    })    
    
    cutoff <- eventReactive(input$pred, {
        cutoff <- if(input$facilityname %in% names(cutoffs)){
            cutoffs[[input$facilityname]]
        } else {
            cutoffs[["Overall"]]
        }
        
        print(cutoff)
        cutoff
    })
    
    output$predText <- renderText({

        if(prediction() > cutoff()[1, 3]){
            sprintf("Highest Risk",
                    paste0(round(cutoff()[1,2]*100, digits = 1), "%"),
                    paste0(round(cutoff()[1,1]*100, digits = 1), "%"))
        } else if(prediction() > cutoff()[2, 3]){
            sprintf("High Risk",
                    paste0(round(cutoff()[2,2]*100, digits = 1), "%"),
                    paste0(round(cutoff()[2,1]*100, digits = 1), "%"))
        } else if(prediction() > cutoff()[3, 3]){
            sprintf("Medium Risk",
                    paste0(round(cutoff()[3,2]*100, digits = 1), "%"),
                    paste0(round(cutoff()[3,1]*100, digits = 1), "%"))
        } else {"Low Risk"}

    })
    
    # Disable Record Prediction button until a prediction is made
    observe({
        if (input$pred == 0) {
            shinyjs::disable("recPred")
        } else {
            shinyjs::enable("recPred")
        }
    })
    
    observeEvent(input$recPred, {
        
        showModal(modalDialog(
            title = "Record Prediction",
            br(),
            "Please enter username and password to record prediction",
            br(),
            textInput("usrnm", "Username", value = ""),
            passwordInput("pswrd", "Password", value = ""),
            actionButton("submitPred", "Submit")
            # actionButton("recPredFinal", "Yes, Record Prediction")
        ))
 
    })
    
    observeEvent(input$submitPred, {
        
        dbConfig <- config::get("database")
        conn <- dbConnect(
            RMariaDB::MariaDB(),
            dbname = dbConfig$dbname,
            host = dbConfig$host,
            port = dbConfig$port,
            username = dbConfig$username,
            password = dbConfig$password,
        )
        user_auth <- dbGetQuery(conn, "SELECT * FROM HomaBayAccess WHERE usernames = ? AND passwords = ?", 
                                params = c(input$usrnm, input$pswrd))
        dbDisconnect(conn)
        

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
        
        dbConfig <- config::get("database")
        conn <- dbConnect(
            RMariaDB::MariaDB(),
            dbname = dbConfig$dbname,
            host = dbConfig$host,
            port = dbConfig$port,
            username = dbConfig$username,
            password = dbConfig$password,
        )

        # conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        df <- data.frame(ID = NA,
                         HTSNumber = input$HTSNumber,
                         AgeAtTest = input$ageattest,
                         MaritalStatus = input$maritalstatus,
                         Gender = input$gender,
                         EverTestedForHIV = input$evertested,
                         MonthsSinceLastTest = input$monthssincelasttest,
                         ClientTestedAs = input$clienttestedas,
                         TestingStrategy = input$testingstrategy,
                         ClientSelfTested = input$clientselftested,
                         TBScreening = input$tbscreening,
                         EntryPoint = input$entrypoint,
                         PatientDisabled = input$patientdisabled,
                         Facility = facilities[facilities$Facility.Name == input$Facility.Name, "Facility.Name"],
                         # month_of_test = predictors()$month_of_test,
                         # dayofweek = predictors()$dayofweek,
                         KeyPopulationType = input$KPtype,
                         Prediction = prediction(),
                         TestResult = 'Pending',
                         TimeofTest = Sys.time())
        dbWriteTable(conn, "HomaBayHTS", df, append = TRUE)
        id_new <- dbGetQuery(conn, "SELECT MAX(ID) FROM HomaBayHTS")
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
            passwordInput("pswrd_res", "Password", value = ""),
            actionButton("submitResult", "Submit")
        ))
    })
    
    observeEvent(input$submitResult, {
        
        dbConfig <- config::get("database")
        conn <- dbConnect(
            RMariaDB::MariaDB(),
            dbname = dbConfig$dbname,
            host = dbConfig$host,
            port = dbConfig$port,
            username = dbConfig$username,
            password = dbConfig$password,
        )
        # conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        user_auth <- dbGetQuery(conn, "SELECT * FROM HomaBayAccess WHERE usernames = ? AND passwords = ?", 
                                params = c(input$usrnm_res, input$pswrd_res))
        dbDisconnect(conn)
        
        if(nrow(user_auth) == 0){
            
            showModal(modalDialog(
                title = "User Not Recognized",
                br(), br(),
                "Sorry, Username and Password not recognized. Please try again."))
        }
        
        if(nrow(user_auth) == 1){
    
        dbConfig <- config::get("database")
        conn <- dbConnect(
            RMariaDB::MariaDB(),
            dbname = dbConfig$dbname,
            host = dbConfig$host,
            port = dbConfig$port,
            username = dbConfig$username,
            password = dbConfig$password,
        )    
        # conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        ids <- dbGetQuery(conn, "SELECT ID FROM HomaBayHTS")
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
    
    # observeEvent(input$recResultFinal, {
    #     
    #     dbConfig <- config::get("database")
    #     conn <- dbConnect(
    #         RMariaDB::MariaDB(),
    #         dbname = dbConfig$dbname,
    #         host = dbConfig$host,
    #         port = dbConfig$port,
    #         username = dbConfig$username,
    #         password = dbConfig$password,
    #     )
    #     # conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
    #     dbExecute(conn, "UPDATE HomaBayHTS SET TestResult = ? where ID = ?", params = c(input$testResult, input$id_input))
    #     dbExecute(conn, "UPDATE HomaBayHTS SET TimeofTest = ? where ID = ?", params = c(as.character(Sys.time()),input$id_input))
    #     dbDisconnect(conn)
    #     
    #     showModal(modalDialog(
    #         title = "Test Result Successfully Recorded",
    #         br(),
    #         "Please press Dismiss to proceed."
    #     ))
    #     
    # })


})
