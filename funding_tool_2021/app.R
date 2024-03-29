#
# Title: Budget Calculator for Pre-K Centers
# Author: Matt Worthington and Lutfi Sun
# Date: July 21, 2021
#
# Acknowledgment: This calculator is based on code provided by Matt Worthington and
# its successor https://davidmc.shinyapps.io/prek-partnership-app/
#

###--- PREP WORKSPACE -------------------------------------------------------------------

## Load Packages
library(shiny)
library(tidyverse)
library(ggthemes)
library(shinyBS)

## Load Data

#--- UI---------------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(HTML("<title>Pre-K Partnership Budget Calculator </title>")),
    navbarPage(
        # Without company logo
        title = tags$a(href='https://www.unitedwayaustin.org/',
                       tags$img(src='UWA-Logo.png', height = "75", width = "275"), color="#fff"),
        
        # Application title
        tabPanel("Partnerships Overview",
                 div(class = "overview",
                     h1("How do Pre-K Partnerships Work?")),
                 hr(),
                 fluidRow(
                     div(class = "explainers",
                         column(width = 12,
                                includeMarkdown("text/revenues_intro.md"))))
                 
        ),
        
        # **Style elements ---------------------------------------
        
        ## Style and header elements
        tabPanel(
            title = "Calculator Tool",
            div(class = "overview-2",
                h1("How Your Center Would Work")),
            id = "nav",
            theme = "navbar-dark bg-dark",
            responsive = TRUE,
            collapsible = TRUE,
            tags$head(
                # Include our custom CSS
                includeCSS("www/styles.css"),
                tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", 
                          integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", 
                          crossorigin="anonymous")
            ),
            br(),
            br(),
            fluidRow(
                div(class = "explainers",
                    column(width = 12, 
                           includeMarkdown("text/disclaimer.md")))),
            br(),
            br(),
            
            ## Sidebar for user selections 
            sidebarLayout(
                sidebarPanel(
                    tags$head(tags$style(
                        type = 'text/css',
                        'form.well { max-height: 800px; padding-right: 30px; overflow-y: auto; }'
                    )),
                    
                    # USER INPUTS ------------------------------------------------------------
                    # PRE TAB - GENERAL INFO -------------------------------------------------
                    # **Center name -----------------------------
                    # User input (not tied to our data at all)
                    textInput("centername", 
                              label = h4("Name of Center"), 
                              placeholder = "Enter A Name..."),
                    
                    tabsetPanel(
                        
                        # TAB 1 - REVENUES TAB ------------------------------------------------------------
                        tabPanel(h4("Annual Revenues"), 
                                 br(),
                                 h4("Classroom Enrollment Information"),
                                 
                                 ## Classroom age
                                 
                                 ## Student composition by age & status 
                                 h4("Payment Sources"),
                                 p("Enter the number of children in each category:"),
                                 
                                 # questions to appear for blended 3- & 4-yo class
                                                 
                                 ## subsidy only, 3yo 
                                 numericInput("subsidy_only3",
                                              "Number of subsidy-only 3-year-old children:",
                                              value = 0),
                                                  
                                 sliderInput("subsidyrate3",
                                             "Daily subsidy rate per 3-year-old:",
                                             min = 30,
                                             max = 50,
                                             step = 0.01,
                                             pre="$",
                                             post="/day",
                                             value = 43.70),
 
                                 ## Pre-K pass, 3yo 
                                 numericInput("prek3",
                                              "Number of 3-year-olds with Pre-K pass:",
                                              value = 0),
                                 
                                 sliderInput("prek3_rate",
                                             "3-year-old Pre-K pass rate from district:",
                                             min = 10,
                                             max = 30,
                                             step = 0.01,
                                             pre="$",
                                             post="/day",
                                             value = 20),
                                 
                                 ## subsidy only, 4yo 
                                 numericInput("subsidy_only4",
                                              "Number of subsidy-only 4-year-old children:",
                                              value = 0),
                                                 
                                 sliderInput("subsidyrate4",
                                             "Daily subsidy rate per 4-year-old:",
                                             min = 30,
                                             max = 50,
                                             step = 0.01,
                                             pre="$",
                                             post="/day",
                                             value = 35.29),
                                                 
                                 ## Pre-K pass, 4yo 
                                 numericInput("prek4",
                                              "Number of 4-year-olds with Pre-K pass:",
                                              value = 0),
                                 
                                 sliderInput("prek4_rate",
                                             "4-year-old Pre-K pass rate from district:",
                                             min = 10,
                                             max = 30,
                                             step = 0.01,
                                             pre="$",
                                             post="/day",
                                             value = 20),
                                 
                                 # questions to appear regardless of classroom age
                                 # pre-K only (don't pay tuition)
                                 numericInput("prekonly",
                                              "Number of pre-K only children:",
                                              value = 0),
                                 # full-pay students
                                 numericInput("enrollfullpay", 
                                              "Number of tuition-only children:", 
                                              value = 10),
                                 
                                 # tuition rate 
                                 sliderInput("tuitionrate",
                                             "Monthly tuition rate per child:",
                                             min = 400,
                                             max = 1500,
                                             pre="$",
                                             post="/mo",
                                             value = 1100),
                                 
                                 ## Pass-through amount slider
                                 sliderInput(inputId = "extra_slider",
                                             label = "Extra funds including full day supplements per year",
                                             min = 0,
                                             max = 30000,
                                             pre="$",
                                             value = 15000),
                                 
                                 ## Hover-over definition labels for each field
                                 bsTooltip("extra_slider", "Any amount of funding that your public school partner is passing through to you. It may be per child, per teacher, per classroom, per month, or per year. Make the calculation on your own for what you would receive for the entire year and enter it here",
                                           "top"),
                                 
                                 bsTooltip("subsidy_only3", "Those who are in the child care subsidy program, but NOT eligible for public school pre-K"),
                                 bsTooltip("prek3", "Those who are in the child care subsidy program AND are eligible for public school pre-K"),
                                 
                                 bsTooltip("subsidy_only4", "Those who are in the child care subsidy program, but NOT eligible for public school pre-K"),
                                 bsTooltip("prek4", "Those who are in the child care subsidy program AND are eligible for public school pre-K"),
                                 
                                 bsTooltip("prekonly", "Those children only eligible for public school pre-K; you are not receiving other payments for their care"),
                                 bsTooltip("enrollfullpay", "Those who are not in the subsidy program and not eligible for pre-K; you only receive payment from tuition")
                        ),
                        
                        # TAB 2 - EXPENSES TAB ------------------------------------------------------------
                        tabPanel(h4("Annual Expenses"),
                                 br(),
                                 
                                 h4("Pre-K Lead Teacher"),
                                 
                                 ## Does the district or the center pay for the lead teacher?
                                 checkboxInput("districtpayteacher", 
                                               label = "Check this box if the school district pays for the lead teacher", 
                                               value = FALSE),
                                 
                                 ## Conditional panel: appears if center pays for lead teacher (i.e., box not checked)
                                 conditionalPanel("!input.districtpayteacher",
                                                  
                                                  # lead teacher rate
                                                  sliderInput("leadrate1",
                                                              label=NULL,
                                                              min = 7.25,
                                                              max = 25,
                                                              step = .25,
                                                              pre = "Rate: $",
                                                              post = " / hr",
                                                              value = 23.5),
                                                  
                                                  # lead teacher hours/week slider
                                                  sliderInput("leadhours1",
                                                              label=NULL,
                                                              min = 20,
                                                              max = 40,
                                                              post = " hrs/wk",
                                                              value = 30),
                                                  
                                                  # lead teacher weeks/year slider
                                                  sliderInput("leadweeks1",
                                                              label=NULL,
                                                              min = 44,
                                                              max = 52,
                                                              post = " wks/yr",
                                                              value = 52),
                                                  
                                                  # benefits + fringe
                                                  hr(),
                                                  h4("Benefits/Fringe Costs"),
                                                  sliderInput("fringerate",
                                                              label=NULL,
                                                              min = 0,
                                                              max = 42,
                                                              step = 1,
                                                              post = "%",
                                                              value = 25)
                                 ),
                                 
                                 ## Assistant Teacher 
                                 hr(),
                                 h4("Assistant Teacher(s)"),
                                 
                                 # number of assistants dropdown
                                 selectInput("assistantselect", 
                                             label = "How many Assistants are there?", 
                                             choices = c("1 Assistant" = 1 , "2 Assistants" = 2)),
                                 
                                 # assistant hourly rate slider
                                 h4("Assistant 1"),
                                 sliderInput("assistrate1",
                                             label = NULL,
                                             min = 7.25,
                                             max = 25,
                                             step = .25,
                                             pre = "$",
                                             post = "/hr",
                                             value = 17.5),
                                 
                                 # assistant hours/wk slider
                                 sliderInput("assisthours1",
                                             label = NULL,
                                             min = 10,
                                             max = 40,
                                             post = " hrs/wk",
                                             value = 30),
                                 
                                 # assistant weeks/year slider
                                 sliderInput("assistweeks1",
                                             label = NULL,
                                             min = 1,
                                             max = 52,
                                             post = " wks",
                                             value = 30),
                                 
                                 ## Conditional panel: appears if "2 Assistants" is selected
                                 conditionalPanel("input.assistantselect == '2'",
                                                  h4("Assistant 2"),
                                                  
                                                  # assistant 2 hourly rate
                                                  sliderInput("assistrate2",
                                                              label = NULL,
                                                              min = 7.25,
                                                              max = 25,
                                                              step = .25,
                                                              pre = "$",
                                                              post = "/hr",
                                                              value = 15),
                                                  
                                                  # assistant 2 hours per week
                                                  sliderInput("assisthours2",
                                                              label = NULL,
                                                              min = 1,
                                                              max = 40,
                                                              post = " hrs/wk",
                                                              value = 20),
                                                  
                                                  # assistant 2 weeks per year
                                                  sliderInput("assistweeks2",
                                                              label = NULL,
                                                              min = 1,
                                                              max = 52,
                                                              post = " wks",
                                                              value = 30)),
                                 
                                 
                                 ## Substitute Teacher 
                                 hr(),
                                 h4("Substitute Teacher"),
                                 p("This Indicates the Hours of Substitute Coverage for Teacher PD and Paid Time-Off"),
                                 
                                 # substitute hourly rate
                                 sliderInput("subrate1",
                                             label = NULL,
                                             min = 7.25,
                                             max = 25,
                                             step = .25,
                                             pre="$",
                                             post="/hr",
                                             value = 20),
                                 
                                 # substitute hours/yr
                                 sliderInput("subhours1",
                                             label = NULL,
                                             min = 265,
                                             max = 420,
                                             ticks = FALSE,
                                             post = " hrs",
                                             value = 265),
                                 
                                 ## Summer Teacher
                                 hr(),
                                 h4("Summer Teacher"),
                                 
                                 # summer teacher hourly rate
                                 sliderInput("sumsubrate1",
                                             label=NULL,
                                             min = 7.25,
                                             max = 25,
                                             step = .25,
                                             pre = "$",
                                             post = "/hr",
                                             value = 20),
                                 
                                 # summer teacher hours/yr
                                 sliderInput("sumsubhours1",
                                             label=NULL,
                                             min = 0,
                                             max = 500,
                                             post = " hrs",
                                             value = 200),
                                 
                                 ## Classroom Supplies
                                 hr(),
                                 h4("Classroom Supplies"),
                                 
                                 # supplies $/yr
                                 sliderInput("supplies1",
                                             label = NULL,
                                             min = 2500,
                                             max = 5500,
                                             step = 500,
                                             pre = "$",
                                             value = 3500),
                                 
                                 ## Food 
                                 hr(),
                                 h4("Food Costs"),
                                 
                                 # food cost/child/day
                                 # only two options: $0 or $2
                                 sliderInput("foodcost",
                                             label = "If your school receives CACFP support, select $0/day/child",
                                             min = 0,
                                             max = 4,
                                             step = 1,
                                             pre = "$",
                                             post = "/day/child",
                                             value = 2),
                                 
                                 ## Overhead 
                                 hr(),
                                 h4("Overhead Costs"),
                                 
                                 # overhead $/yr
                                 sliderInput("overhead1",
                                             label=NULL,
                                             min = 20000,
                                             max = 40000,
                                             step = 100,
                                             pre = "$",
                                             value = 30000))),
                    
                    ## Hover-over definition labels for each field
                    bsTooltip("sumsubrate1", "If the lead pre-K teacher is off in the summer and you need a replacement lead teacher"),
                    bsTooltip("supplies1", "Estimate your yearly cost for classroom supplies"),
                    bsTooltip("overhead1", "This includes items that must be paid regardless of how your business is doing (rent, utilities, maintenance, accounting, taxes, insurance, administrative staff, etc.). Estimate your overhead costs for this pre-K classroom only")
                    #bsTooltip("fringerate", "Add together the annual cost of all employee benefits and payroll taxes paid, and divide by the annual wages paid. This gives you a percentage you can input here")
                ),
                
                ## Display plot
                mainPanel(
                    downloadButton('downloadggplot', "Download This Model"),
                    br(),
                    hr(),
                    plotOutput("mod1Plot", width = "100%", height = "800px")
                )
        )
    )))
        

###--- SERVER---------------------------------------------------------------------------------
server <- function(input, output, session) {
    
    # Reset hidden fields (i.e., from conditionals) to 0
    
    ###------ FUNCTION: CALCULATE REV/EX, MAKE PLOT -------###
    
    output$mod1Plot <- renderPlot({
        
        # Make a local variable that is all students
        enrollees <-    input$subsidy_only3 + input$prek3 +
                        input$subsidy_only4 + input$prek4 +
                        input$prekonly + input$enrollfullpay
        
        # Local variables to group "both" vs single age group classes
        subsidyonly3    <- input$subsidy_only3
        prek3           <- input$prek3
        
        subsidyrate3    <- input$subsidyrate3
        prek3_rate      <- input$prek3_rate
        
        subsidyonly4    <- input$subsidy_only4
        prek4           <- input$prek4
        
        subsidyrate4    <- input$subsidyrate4
        prek4_rate      <- input$prek4_rate
        
        # make a local variable- if districtpayteacher is selected, will remove teacher expense from formula
        districtpay <- ifelse(input$districtpayteacher == TRUE, 0, 1)
        leadhours <- ifelse(input$districtpayteacher == TRUE, 0, input$leadhours1)
        leadrate  <- ifelse(input$districtpayteacher == TRUE, 0, input$leadrate1)
        leadweeks <- ifelse(input$districtpayteacher == TRUE, 0, input$leadweeks1)
        
        ## CONDITION: 1 Assistant
        if (input$assistantselect == "1" ) { 
            
            # Make a local tibble including calculated values for revenue/expenses
            mod1 <- tibble(
                
                # EXPENSES - teacher salaries + supplies + overhead + student food + fringe benefits
                Expenses = c(                                                                 
                    (input$leadrate1 * input$leadhours1 * input$leadweeks1 * districtpay) +  # Teacher Annual Salary (Rate*Hours*Weeks). = 0 if district pays
                        (input$assistrate1 * input$assisthours1 * input$assistweeks1) +      # Assistant 1 Annual Salary (Rate*Hours*Weeks)
                        (input$subrate1 * input$subhours1) +                                 # Substitute Teacher Wages (Rate*Hours)
                        (input$sumsubrate1 * input$sumsubhours1) +                           # Summer Substitute Wages (Rate*Hours)
                        input$supplies1 +                                                    # Supplies Costs (2500-5500)
                        input$overhead1 +                                                    # Overhead Costs (20K-40K)
                        (enrollees * input$foodcost * 262) +                                 # Food Costs (Enrollment*DailyRate*262 days/year). daily rate = 0 if CACFP support, otherwise = 2
                        (input$leadrate1 * input$leadhours1 * input$leadweeks1 * (input$fringerate/100) * districtpay)),  # Fringe Benefits for Teacher (= 0 if district pays)
                
                # REVENUE - student tuition (full-pay and subsidy, subsidy pay dependent on student age/enrollment status)
                Revenue =
                    c(
                        input$extra_slider +                            # Pass Through Amount
                            (input$enrollfullpay * input$tuitionrate * 12) +  # Full-Pay Student Revenue: NumberFullPayStudents*MonthlyTuition*12
                            (subsidyonly3 * subsidyrate3 * 262) +             # Subsidy only 3yo: NumberSubsidyOnly * PSFT daily rate * 262 days/yr
                            (subsidyonly4 * subsidyrate4 * 262)               # Subsidy only 4yo: NumberSubsidyOnly * PSFT daily rate * 262 days/yr
                    )
            )
            
            
            mod1 <- mod1 %>% gather(variable, value, 1:2)
            
            # MAKE PLOT
            gg_mod1 <- mod1 %>% ggplot() +
                geom_bar(aes(x = variable, y = value, fill = variable), stat= "identity",alpha = 0.95) +
                geom_text(aes(x=variable, y=value, label=scales::dollar(value, accuracy=1)), nudge_y = -30000, size = 8, fontface="bold") +
                theme_bw(base_size = 22) +
                theme(plot.subtitle = element_text(size = 12)) +
                coord_flip() +
                scale_y_continuous(labels=scales::dollar_format(scale=.001, suffix = "k")) +
                guides(fill=FALSE,color=FALSE)+
                scale_fill_manual(values = c("#ff2700", "#008fd5")) +
                labs(title= paste(input$centername),
                     y=NULL,
                     x=NULL,
                     # Plot subtitle reactively populated with user input
                     subtitle = paste("Scenario: ",input$modeltype,"\n",
                                      "\n",
                                      "GENERAL INFO:\n",
                                      "Name of Center: ",input$centername, "\n",
                                      "\n",
                                      "ANNUAL REVENUES:\n",
                                      "Total Number of Children Enrolled: " , enrollees , "\n",
                                      "Subsidy-only 3-yos enrolled: ",subsidyonly3, " @ $",subsidyrate3,"/child/day\n",
                                      "Subsidy-only 4-yos enrolled: ",subsidyonly4, " @ $",subsidyrate4,"/child/day\n",
                                      "Pre-K + subsidy eligible 3-yos enrolled: ",prek3, " @ $",prek3_rate,"/child/day\n",
                                      "Pre-K + subsidy eligible 4-yos enrolled: ",prek4, " @ $",prek4_rate,"/child/day\n",
                                      "Pre-K only children enrolled: ",input$prekonly, " @ $0/day\n",
                                      "Tuition-only children enrolled: ",input$enrollfullpay, " @ $",input$tuitionrate*12, "/child/year\n",
                                      "Additional Funds: $",input$extra_slider, "/yr\n",
                                      "\n",
                                      "ANNUAL EXPENSES:\n",
                                      "Pre-K lead teacher: $",leadrate, collapse=NULL,sep="","/hr | ",leadhours,"hrs/wk | ",leadweeks,"wks/yr\n",
                                      "Assistant teacher: $",input$assistrate1,"/hr | ",input$assisthours1,"hrs/wk | ",input$assistweeks1,"wks/yr\n",
                                      "Substitute teacher: $",input$subrate1, "/hr @ ",input$subhours1," hrs\n",
                                      "Summer teacher: $",input$sumsubrate1,"/hr  @ ",input$sumsubhours1," hrs\n",
                                      "Classroom supplies: $",input$supplies1,"\n",
                                      "Food costs: $",input$foodcost,"/child/day\n",
                                      "Overhead: $",input$overhead1, "\n",
                                      "Benefits + fringe: ",input$fringerate, "% for 1 FTE"))
            ggsave("plot.pdf", gg_mod1, device = "pdf")
            gg_mod1
            
            ## CONDITION: 2 Assistants
            
        } else if (input$assistantselect == "2") {
            
            mod2 <- tibble(
                
                # EXPENSES - teacher salaries + supplies + overhead + student food + fringe benefits
                Expenses = c(                                                                 
                    (input$leadrate1 * input$leadhours1 * input$leadweeks1 * districtpay) +  # Teacher Annual Salary (Rate*Hours*Weeks). = 0 if district pays
                        (input$assistrate1 * input$assisthours1 * input$assistweeks1) +      # Assistant 1 Annual Salary (Rate*Hours*Weeks)
                        (input$assistrate2 * input$assisthours2 * input$assistweeks2) +      # Assistant 1 Annual Salary (Rate*Hours*Weeks)
                        (input$subrate1 * input$subhours1) +                                 # Substitute Teacher Wages (Rate*Hours)
                        (input$sumsubrate1 * input$sumsubhours1) +                           # Summer Substitute Wages (Rate*Hours)
                        input$supplies1 +                                                    # Supplies Costs (2500-5500)
                        input$overhead1 +                                                    # Overhead Costs (20K-40K)
                        (enrollees * input$foodcost * 205) +                                 # Food Costs (Enrollment*DailyRate*205 days/year). daily rate = 0 if CACFP support, otherwise = 2
                        (input$leadrate1 * input$leadhours1 * input$leadweeks1 * (input$fringerate/100) * districtpay)),  # Fringe Benefits for Teacher (= 0 if district pays)
                
                # REVENUE - student tuition (full-pay and subsidy, subsidy pay dependent on student age/enrollment status)
                Revenue =
                    c(
                        input$extra_slider +                            # Pass Through Amount
                            (input$enrollfullpay * input$tuitionrate * 12) +  # Full-Pay Student Revenue: NumberFullPayStudents*MonthlyTuition*12
                            
                            (subsidyonly3 * subsidyrate3 * 205) +             # Subsidy only 3yo: NumberSubsidyOnly * PSFT daily rate * 205 days/yr
                            (prek3 * prek3_rate * 175) +                      # Prek subsidy eligible 3yo: NumberPrekSubsidyEligible * PSFT daily rate * 262 days/yr
                            
                            (subsidyonly4 * subsidyrate4 * 205) +             # Subsidy only 4yo: NumberSubsidyOnly * PSFT daily rate * 205 days/yr
                            (prek4 * prek4_rate * 175)                        # Prek subsidy eligible 3yo: NumberPrekSubsidyEligible * PSFT daily rate * 262 days/yr
                    )
            )
            
            mod2 <- mod2 %>% gather(variable, value, 1:2)
            
            # MAKE PLOT
            gg_mod2 <- mod2 %>% ggplot() +
                geom_bar(aes(x=variable, y = value, fill = variable), stat= "identity",alpha = 0.95) +
                geom_text(aes(x=variable, y=value, label=scales::dollar(value, accuracy=1)), nudge_y = -22000, size = 8, fontface="bold") +
                theme_bw(base_size = 22) +
                theme(plot.subtitle = element_text(size = 12)) + 
                coord_flip() +
                scale_y_continuous(labels=scales::dollar_format(scale=.001, suffix = "k")) +
                guides(fill=FALSE,color=FALSE)+
                scale_fill_manual(values = c("#ff2700", "#008fd5")) +
                labs(title= paste(input$centername),
                     y=NULL,
                     x=NULL,
                     # Plot subtitle reactively populated with user input
                     subtitle = paste("Scenario: ",input$modeltype,"\n",
                                      "\n",
                                      "GENERAL INFO:\n",
                                      "Name of Center: ",input$centername, "\n",
                                      "\n",
                                      "ANNUAL REVENUES:\n",
                                      "Total Number of Children Enrolled: " , enrollees , "\n",
                                      "Subsidy-only 3-yos enrolled: ",subsidyonly3, " @ $",subsidyrate3,"/child/day\n",
                                      "Subsidy-only 4-yos enrolled: ",subsidyonly4, " @ $",subsidyrate3,"/child/day\n",
                                      #"Pre-K + subsidy eligible 3-yos enrolled: ",subsidyprek3, " @ $",subsidyrate3,"/child/day\n",
                                      #"Pre-K + subsidy eligible 4-yos enrolled: ",subsidyprek4, " @ $",subsidyrate4,"/child/day (school year), $",subsidyrate3,"/child/day (summer)\n",
                                      "Pre-K only children enrolled: ",input$prekonly, " @ $0/day\n",
                                      "Tuition-only children enrolled: ",input$enrollfullpay, " @ $",input$tuitionrate*12, "/child/year\n",
                                      "Pre-K pass-through amount: $",input$extra_slider, "/yr\n",
                                      "\n",
                                      "ANNUAL EXPENSES:\n",
                                      "Pre-K lead teacher: $",leadrate, collapse=NULL,sep="","/hr | ",leadhours,"hrs/wk | ",leadweeks,"wks/yr\n",
                                      "Assistant teacher 1: $",input$assistrate1,"/hr | ",input$assisthours1,"hrs/wk | ",input$assistweeks1,"wks/yr\n",
                                      "Assistant teacher 2: $",input$assistrate2,"/hr | ",input$assisthours2,"hrs/wk | ",input$assistweeks2,"wks/yr\n",
                                      "Substitute teacher: $",input$subrate1, "/hr @ ",input$subhours1," hrs\n",
                                      "Summer teacher: $",input$sumsubrate1,"/hr  @ ",input$sumsubhours1," hrs\n",
                                      "Classroom supplies: $",input$supplies1,"\n",
                                      "Food costs: $",input$foodcost,"/child/day\n",
                                      "Overhead: $",input$overhead1, "\n",
                                      "Benefits + fringe: ",input$fringerate, "% for 1 FTE"))
            
            ggsave("plot.pdf", gg_mod2, device = "pdf")
            
            gg_mod2
        }  
    })
    
    # Make plot a pdf for download 
    output$downloadggplot <- downloadHandler(
        filename = function() {
            "plot.pdf"
        },
        content = function(file) {
            file.copy("plot.pdf", file, overwrite=TRUE)
            
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)