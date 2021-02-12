# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinythemes::shinytheme("spacelab"),

    # Application title
    titlePanel(div(strong("Homa Bay County HTS Client Assessment"), style = "color:black")),

    # Sidebar with a slider input for number of bins
    fluidRow(div("Welcome to the National HTS Client Assesment Portal. 
          This platform is intended to help you make a better decision on
                 who to test, increasing the efficiency of use of test kits. Kindly proceed to fill in the bio details of your patient to recieve
                 the prediction.", style = "color:black", style = "font-size:5000px")),
    br(),
    
    sidebarLayout(
        sidebarPanel(
                  selectInput("sitecode",
                              "Facility Name",
                              choices = c("", facilities$Name),
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
                                choices = c("", as.character(unique(dat$MaritalStatus))),
                                selected = NULL),
                    
                        selectInput("KPtype",
                                    "Population type:",
                                    choices = c("", population$PopulationType),
                                    selected = NULL),
            # selectInput("patientdisabled",
            #             "Client Disability Status",
            #             choices = c("", as.character(unique(dat$PatientDisabled))),
            #             selected = NULL),
            selectInput("clienttestedas",
                        "Client Testing As",
                        choices = c("", as.character(unique(dat$ClientTestedAs))),
                        selected = NULL),
            # selectInput("entrypoint",
            #             "Entry Point",
            #             choices = c("", as.character(unique(dat$EntryPoint))),
            #             selected = NULL),
            selectInput("testingstrategy",
                        "Testing Strategy",
                        choices = c("", testingstrategy$Name),
                        selected = NULL),
            selectInput("tbscreening",
                        "TB Screening Results",
                        choices = c("", as.character(unique(dat$TBScreening))),
                        selected = NULL),
            selectInput("clientselftested",
                        "Ever had an HIV Self Test",
                        choices = c("", as.character(unique(dat$ClientSelfTested))),
                        selected = NULL),
            selectInput("evertested",
                        "Ever Tested for HIV by a HealthWorker",
                        choices = c("", as.character(unique(dat$EverTestedForHIV))),
                        selected = NULL),
            conditionalPanel(
                condition = "input.evertested == 'Yes'",
                numericInput("monthssincelasttest",
                             "Months Since Last Test",
                             value = 0,
                             min = 0,
                             max = 50))
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 6,
            actionButton('pred', 'Generate Prediction',class="btn-primary"),
            br(),
            br(),
            verbatimTextOutput("predText"),
            br(),
            br(),
            actionButton('recPred', 'Save Prediction', class = "btn-primary"),
            br(),
            br(),
            br(),
            br(),
            actionButton('recResult', 'Record Test Result',class="btn-primary")
        )
        )
))
