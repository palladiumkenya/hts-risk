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
    "Kindly proceed to fill in the bio details of your patient to recieve
    the prediction.",
    br(),
    br(),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("facilityname",
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
                        choices = c("", dat_unique$Gender),
                        selected = NULL),
            selectInput("maritalstatus",
                        "Marital Status",
                        choices = c("", dat_unique$MaritalStatus, "Single"),
                        selected = NULL),
            selectInput("KPtype",
                        "Population type:",
                        choices = c("", dat_unique$KeyPopulationType, "PWID"),
                        selected = NULL),
            selectInput("patientdisabled",
                        "Patient Disabled",
                        choices = c("", dat_unique$PatientDisabled),
                        selected = NULL),
            selectInput("clienttestedas",
                        "Client Testing As",
                        choices = c("", dat_unique$ClientTestedAs),
                        selected = NULL),
            selectInput("entrypoint",
                        "Entry Point",
                        choices = c("", dat_unique$EntryPoint),
                        selected = NULL),
            selectInput("testingstrategy",
                        "Testing Strategy",
                        choices = c("", dat_unique$TestingStrategy, "NP", "HP", "PNS"),
                        selected = NULL),
            selectInput("tbscreening",
                        "TB Screening Results",
                        choices = c("", dat_unique$TBScreening),
                        selected = NULL),
            selectInput("clientselftested",
                        "Ever had an HIV Self Test",
                        choices = c("", dat_unique$ClientSelfTested),
                        selected = NULL),
            selectInput("evertested",
                        "Ever Tested for HIV by a HealthWorker",
                        choices = c("", dat_unique$EverTestedForHIV),
                        selected = NULL),
            conditionalPanel(
                condition = "input.evertested == 'Yes'",
                numericInput("monthssincelasttest",
                             "Months Since Last Test",
                             value = 0,
                             min = 0,
                             max = 50)),
            selectInput("eligibility",
                        "Client Eligible for Testing",
                        choices = c("", "Eligible", "Not Eligible"), 
                        selected = NULL),
                        conditionalPanel(
                            condition = "input.eligibility== 'Eligible'",
                            textInput("htsnumber",
                                      "Client Number",
                                      value =0)
                            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(#width = 6,
            actionButton('pred', 'Generate Prediction',class="btn-primary"),
            br(),
            br(),
            verbatimTextOutput("predText"),
            br(),
            br(),
            actionButton('recPred', 'Save Prediction',class="btn-primary")
        )
    )
))
