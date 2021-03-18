# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinythemes::shinytheme("spacelab"),
    shinyjs::useShinyjs(),
                  
    # Application title
    titlePanel("Homa Bay County HTS Client Assessment"),

    # Sidebar with a slider input for number of bins
    "Welcome to the National HTS Client Assesment Portal. 
          This platform is intended to help you make a better decision on
                 who to test, increasing the efficiency of use of test kits.", 
    br(),
    br(),
    
    "Kindly proceed to fill in the bio details of your patient to recieve
    the prediction.",
    br(),
    br(),

    sidebarLayout(
        sidebarPanel(
          selectInput("sitecode",
                      "Facility Name",
                      choices = c("", facilities$Facility.Name),
                      selected = NULL),
            numericInput("ageattest",
                         "Client's Age",
                         value = 0,
                         min = 0,
                         max = 100),
            selectInput("gender",
                      "Gender",
                      choices = c("", as.character(unique(dat$Gender))),
                      selected = NULL),
            selectInput("maritalstatus",
                        "Marital Status",
                        choices = c("", as.character(unique(dat$MaritalStatus)), "Single"),
                        selected = NULL),
            selectInput("KPtype",
                        "Key Population type:",
                        choices = c("", population$PopulationType),
                        selected = NULL),
            # selectInput("patientdisabled",
            #             "Patient Disabled",
            #             choices = c("", as.character(unique(dat$PatientDisabled))),
            #             selected = NULL),
            selectInput("clienttestedas",
                        "Client Testing As",
                        choices = c("", as.character(unique(dat$ClientTestedAs))),
                        selected = NULL),
            selectInput("entrypoint",
                        "Entry Point",
                        choices = c("", entrypoint$Name),
                        selected = NULL),
            selectInput("testingstrategy",
                        "Testing Strategy",
                        choices = c("",testingstrategy$Name),
                        selected = NULL),
            selectInput("tbscreening",
                        "TB Screening Results",
                        choices = c("", as.character(unique(dat$TBScreening))),
                        selected = NULL),
            selectInput("clientselftested",
                        "Ever had an HIV Self test",
                        choices = c("", as.character(unique(dat$ClientSelfTested))),
                        selected = NULL),
            selectInput("evertested",
                        "Ever Tested for HIV by a Health Worker",
                        choices = c("", as.character(unique(dat$EverTestedForHIV))),
                        selected = NULL),
            conditionalPanel(
                condition = "input.evertested == 'Yes'",
                numericInput("monthssincelasttest",
                             "Months Since Last Test",
                             value = 0,
                             min = 0,
                             max = 50))
        #),
		
		
		 ),
		 
		 
        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
              
            column(4,"FOR BEHAVIOURAL GUIDED ELIGIBILITY SCREENING DESCISION",
                   br(),
                   br(),
            selectInput("lasthivtest", 
                        "Time since last HIV test ", 
                        choices = c("","Less 12 months", "More than 12 Months"),
                        selected = NULL),
            conditionalPanel(
              condition = "input.lasthivtest == 'Less 12 months'",
              selectInput("sexlast12months", 
                          "Had sex in the past 12 months? :", 
                          choices = c("","Yes", "No"),
                          selected =NULL)),
            conditionalPanel(
              condition = "input.sexlast12months == 'Yes'",
              selectInput("numbersexpartner", 
                          "Number of Sexual Partners :", 
                          choices = c("","One", "More than one"),
                          selected = NULL)),
            conditionalPanel(
              condition = "input.sexlast12months == 'Yes'",
              selectInput("knowhivstatussexpartner", 
                        "Knowledge of HIV status of Sexual Partners? :", 
                        choices = c("","Yes", "No"),
                        selected = NULL)),
            conditionalPanel(
              condition = "input.knowhivstatussexpartner == 'Yes'",
              selectInput("hivstatussexpartner", 
                          "What is HIV status of the sexual partners? :", 
                          choices = c("","Verifiable HIV Negative", 
                                      "Unverifiable HIV Negative", "HIV positive"),
                          selected = NULL))
            ),
          column(4,
                actionButton('pred', 'Generate Prediction',class="btn-primary"),
                      br(),
                      br(),
                verbatimTextOutput("predText"),
                      br(),
                      br(),
                actionButton('recPred', 'Save Prediction',class="btn-primary"),
                      br(),
                      br(),
                      br(),
                      br(),
                actionButton('recResult', 'Record Test Result',class="btn-primary")
          ) 
             
        )
    ))
))
