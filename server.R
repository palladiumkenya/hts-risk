# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    predictors <- eventReactive(input$pred, {
        
        validate(need(input$ageattest, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$KPtype, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$maritalstatus, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$gender, "Cannot generate prediction: missing at least one input value"))
        # validate(need(input$patientdisabled, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$evertested, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$monthssincelasttest, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$clienttestedas, "Cannot generate prediction: missing at least one input value"))
        # validate(need(input$entrypoint, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$testingstrategy, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$tbscreening, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$clientselftested, "Cannot generate prediction: missing at least one input value"))
        validate(need(input$sitecode, "Cannot generate prediction: missing at least one input value"))
    
        # Get sitecode
        facility_sitecode <- facilities[facilities$Name == input$sitecode, ]
        df_sc <- merge(sc_info, facility_sitecode, by.x = "Sitecode", by.y = "Code") %>% select(-Sitecode, -Name)
        
        # Get Population Type
        population_type <- population[population$PopulationType == input$KPtype, ]
        
        ## Get Entry Point
        #entry_point <- entrypoint[entrypoint$Name == input$entrypoint, ]
        
        # Get Testing Strategy
        testing_strategy <- testingstrategy[testingstrategy$Name == input$testingstrategy, ]

        df <- data.frame(AgeAtTest = as.numeric(input$ageattest),
                   KeyPopulationType = factor(population_type$PopCode, levels = levels(dat$KeyPopulationType)),
                   MaritalStatus = factor(input$maritalstatus, levels = levels(dat$MaritalStatus)),
                   Gender = factor(input$gender, levels = levels(dat$Gender)),
                   # PatientDisabled = factor(input$patientdisabled, levels = levels(dat$PatientDisabled)),
                   EverTestedForHIV = factor(input$evertested, levels = levels(dat$EverTestedForHIV)),
                   MonthsSinceLastTest = as.numeric(input$monthssincelasttest),
                   ClientTestedAs = factor(input$clienttestedas, levels = levels(dat$ClientTestedAs)),
                   #EntryPoint = factor(entry_point$ID, levels = levels(dat$EntryPoint)),
                   TestingStrategy = factor(testing_strategy$ID, levels = levels(dat$TestingStrategy)),
                   TBScreening = factor(input$tbscreening, levels = levels(dat$TBScreening)),
                   ClientSelfTested = factor(input$clientselftested, levels = levels(dat$ClientSelfTested)),
                   # Sitecode = factor(facility_sitecode$Code, levels = levels(dat$Sitecode)),
                   # Keph.level = factor(facility_sitecode$Keph.level, levels = levels(dat$Keph.level)),
                   # Facility.type = factor(facility_sitecode$Facility.type, levels = levels(dat$Facility.type)),
                   # Owner.type = factor(facility_sitecode$Owner.type, levels = levels(dat$Owner.type)),
                   # Open_whole_day = factor(facility_sitecode$Open_whole_day, levels = levels(dat$Open_whole_day)),
                   # Open_public_holidays = factor(facility_sitecode$Open_public_holidays, levels = levels(dat$Open_public_holidays)),
                   # Open_weekends = factor(facility_sitecode$Open_weekends, levels = levels(dat$Open_weekends)),
                   month_of_test = factor(month(Sys.time()), levels = levels(dat$month_of_test)),
                   dayofweek = factor(wday(Sys.time()), levels = levels(dat$dayofweek)))
        
        df <- cbind(df, df_sc)
        # print(df)
        
        #showModal(modalDialog(
            #title = "Prediction Generated"
        #))

        df
        
    })
    
    prediction <- reactive({
        print(predict(mod, newdata=predictors(), type = "prob"))
        predict(mod, newdata=predictors(), type = "prob")
        
    })    
    
    output$predText <- renderText({
        
        if(prediction()[, 1] > THRESH_75[[3]]){
            sprintf("Highest Risk  Test the Client \n %s of patients with this risk score or higher tested positive. 
                    These patients account for %s of all positive test results",
                    THRESH_75[[2]], THRESH_75[[1]])
        } else if(prediction()[, 1] > THRESH_50[[3]]){
            sprintf("High Risk  Test the Client \n %s of patients with this risk score or higher tested positive. 
                    These patients account for %s of all positive test results",
                    THRESH_50[[2]], THRESH_50[[1]])
        } else if(prediction()[, 1] > THRESH_25[[3]]){
            sprintf("Medium Risk  Test the Client  \n %s of patients with this risk score or higher tested positive. 
                    These patients account for %s of all positive test results",
                    THRESH_25[[2]], THRESH_25[[1]])
        } else {"Low Risk  Do Not Test the Client"}

    })
    
    observeEvent(input$recPred, {
        
        showModal(modalDialog(
            title = "Record Prediction",
            br(),
            "Please enter username and password to record prediction",
            br(),
            textInput("usrnm", "Username", value = ""),
            passwordInput("pswrd", "Password", value = ""),
            actionButton("submitPred", "Submit", class = "btn-primary")
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
        
        # conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        # user_auth <- dbGetQuery(conn, "SELECT * FROM SiayaAccess WHERE usernames = ? AND passwords = ?", 
        #                         params = c(input$usrnm, input$pswrd))
        # dbDisconnect(conn)
        
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
            actionButton("recPredFinal", "Record Prediction", class = "btn-primary")
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
        print(names(predictors()))
        print(predictors()$AgeAtTest)
        
        # print(dim(predictors()))
        print(class(predictors()))
        # conn <- dbConnect(RSQLite::SQLite(), "HTS.db")
        df <- data.frame(ID = NA,
                         AgeAtTest = predictors()$AgeAtTest,
                         MaritalStatus = predictors()$MaritalStatus,
                         Gender = predictors()$Gender,
                         EverTestedForHIV = predictors()$EverTestedForHIV,
                         MonthsSinceLastTest = predictors()$MonthsSinceLastTest,
                         ClientTestedAs = predictors()$ClientTestedAs,
                         # EntryPoint = predictors()$EntryPoint,
                         TestingStrategy = predictors()$TestingStrategy,
                         ClientSelfTested = predictors()$ClientSelfTested,
                         TBScreening = predictors()$TBScreening,
                         Sitecode = facilities[facilities$Name == input$sitecode, 'Code'],
                         month_of_test = predictors()$month_of_test,
                         dayofweek = predictors()$dayofweek,
                         KeyPopulationType = predictors()$KeyPopulationType,
                         Prediction = prediction()[, 1],
                         TestResult = 'Pending',
                         TimeofTest = 'Pending')
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
            actionButton("submitResult", "Submit", class = "btn-primary")
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
            actionButton('recResultFinal', 'Record Test Result', class = "btn-primary")
        ))
        }
    })
    
    observeEvent(input$recResultFinal, {
        
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
        dbExecute(conn, "UPDATE HomaBayHTS SET TestResult = ? where ID = ?", params = c(input$testResult, input$id_input))
        dbExecute(conn, "UPDATE HomaBayHTS SET TimeofTest = ? where ID = ?", params = c(as.character(Sys.time()),input$id_input))
        dbDisconnect(conn)
        
        showModal(modalDialog(
            title = "Test Result Successfully Recorded",
            br(),
            "Please press Dismiss to proceed."
        ))
        
    })


})
