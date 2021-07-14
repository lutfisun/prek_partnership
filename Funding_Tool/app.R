#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(hrbrthemes)
library(ggthemes)
library(showtext)
library(Cairo)



font_add_google("Roboto", "roboto", regular.wt = 400, bold.wt = 900)
showtext_auto()

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(HTML("<title>Pre-K Partnerships: A Success by 6 / AISD Collaboration </title>"),
              tags$link(rel="stylesheet", type="text/css", href="https://cloud.typography.com/6771036/7188012/css/fonts.css")),
    navbarPage(
     #Without company logo
    title = tags$a(href='http://www.unitedwayaustin.org',
                       tags$img(src='logo.png', width='40%'), color="#fff",
                       'Pre-K Partnerships'),
    # Application title
    tabPanel("Partnerships Overview",
             # tags$head(HTML("<title>UWATX Pre-K Project</title>")),
             div(class = "overview",
                 h1(" How do Pre-K Partnerships Work?")),
             hr(),
             fluidRow(
                 div(class = "explainers",
                 column(width = 12, 
                        HTML("<h2 style='color:#005191; padding: 3px 5px 5px 0px; display:block'><i class='fa fa-binoculars'></i>  Overview</h2>"),
                        includeMarkdown("text/revenues_intro.md"))))
             # hr(),
             # fluidRow(
             #     div(class = "explainers",
             #         column(width = 12, 
             #                HTML("<h2 style='color:#005191; padding: 3px 5px 5px 0px; display:block'><i class='fa fa-users'></i>  Teacher Recruitment</h2>"),
             #                includeMarkdown("text/filler_text.md")))),
             # fluidRow(
             #     column(width = 5, offset = 1,
             #            h2("Option 1: CDC employs the PreK teachers"),
             #            h3("Center Does Not Contract with UWATX"),
             #            hr(),
             #            includeMarkdown("text/option1.md")),
             #     
             #     column(width = 5, 
             #            h2("Option 2: UWATX employs the PreK teacher"),
             #            h3("Center contracts with UWATX"),
             #            hr(),
             #            includeMarkdown("text/option2.md"))
             # )
             ),
    # TAB 1 - MODEL 1 ---------------------------------------------------------
    
    
    tabPanel(
        title= "Modeling Tool",
        div(class = "overview-2",
            h1(" How Your Center Would Work")),
        id = "nav",
        # theme = "navbar-dark bg-dark",
        responsive = TRUE,
        collapsible = TRUE,
        tags$head(# Include our custom CSS
            includeCSS("www/styles.css"),
            tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")
        ),
        br(),
        br(),
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                tags$head(tags$style(
                    type = 'text/css',
                    'form.well { max-height: 800px; padding-right: 30px; overflow-y: auto; }'
                )),
                
                textInput("centername", label = h4("Name of Center"), placeholder = "Enter A Name..."),
                selectInput("modeltype", label = h4("Select A Model"), 
                            choices = list("CDC-Based Employee", "UWATX-Based Employee"), 
                            selected = "CDC-Based Employee"),
                tabsetPanel(
                    # TAB 2 - REVENUES TAB ------------------------------------------------------------
                    tabPanel(h4("Revenues"), 
                             br(),
                             # **Full-Pay Students ---------------------------------------
                             h4("Enrollment Information"),
                             numericInput("enrollfullpay", 
                                          "Full-Pay Students Enrolled:", 
                                          value = 10),
                             # **Tuition Rate Students ---------------------------------------
                             sliderInput("tuitionrate",
                                         "Annual Tuition Rate:",
                                         min = 5000,
                                         max = 15000,
                                         pre="$",
                                         post="/yr",
                                         value = 14000),
                             # **CCS-Pay Students ---------------------------------------
                             numericInput("enrollccs",
                                          "CCS Students Enrolled:",
                                          value = 10),
                             h4("Subsidy Information"),
                             p("Students receiving subsidies are given $8,872 per child + a $3,000 pre-k subsidy that is multiplied by 90% of your subsidize enrollment population."),
                             # **Enrollment Percentage ---------------------------------------
                             hr(),
                             h4("Percentage of Children Enrolled in Pre-K Classrooms"),
                             sliderInput("enrollpct",
                                         label=NULL,
                                         min = 1,
                                         max = 100,
                                         step=1,
                                         post="%",
                                         value = 90)
                    ),
                    
                    # EXPENSES TAB ------------------------------------------------------------
                    tabPanel(h4("Expenses"),
                             br(),
                             # **Lead PreK Teacher ---------------------------------------
                             h4("Pre-K Teacher"),
                             # withMathJax(),
                             # helpText('$$Salary = Rate*Hours*Weeks$$'),
                             # p("Approved Hourly Rate x Hours Worked Each Week x Weeks Worked Each Year"),
                             conditionalPanel("input.modeltype == 'CDC-Based Employee'",
                                              sliderInput("leadrate1",
                                                          label=NULL,
                                                          min = 18,
                                                          max = 25,
                                                          step = .25,
                                                          pre = "Rate: $",
                                                          post = " / hr",
                                                          value = 23.5)),
                             # **Conditional Pre-K Teacher Options -----------------------------------------
                             conditionalPanel("input.modeltype == 'UWATX-Based Employee'",
                                              sliderInput("leadrate2",
                                                          label=NULL,
                                                          min = 20,
                                                          max = 25,
                                                          step = .25,
                                                          pre = "Rate: $",
                                                          post = " / hr",
                                                          value = 23.5)),
                             sliderInput("leadhours1",
                                         label=NULL,
                                         min = 20,
                                         max = 40,
                                         # pre = "Weekly Hours: ",
                                         post = " hrs/wk",
                                         value = 30),
                             sliderInput("leadweeks1",
                                         label=NULL,
                                         min = 44,
                                         max = 52,
                                         step = 8,
                                         ticks=0,
                                         # pre = "Weeks Worked Each Year: ",
                                         post = " wks/yr",
                                         value = 52),
                             # **Assistant Teacher ---------------------------------------
                             hr(),
                             h4("Assistant Teacher(s)"),
                             selectInput("assistantselect", label = "How many Assistants are there?", 
                                         choices = list("1 Assistant" = 1, "2 Assistants" = 2), 
                                         selected = NULL),
                             h4("Assistant 1"),
                             sliderInput("assistrate1",
                                         label=NULL,
                                         min = 10,
                                         max = 20,
                                         step = .25,
                                         pre = "$",
                                         post = "/hr",
                                         value = 17.5),
                             sliderInput("assisthours1",
                                         label=NULL,
                                         min = 10,
                                         max = 40,
                                         post = " hrs",
                                         value = 30),
                             sliderInput("assistweeks1",
                                         label=NULL,
                                         min = 1,
                                         max = 52,
                                         post = " wks",
                                         value = 30),
                             conditionalPanel("input.assistantselect == 1"),
# **Conditional Assistant Options -----------------------------------------
                             conditionalPanel("input.assistantselect == 2",
                                              h4("Assistant 2"),
                                              sliderInput("assistrate2",
                                                          label=NULL,
                                                          min = 10,
                                                          max = 20,
                                                          step = .25,
                                                          pre = "$",
                                                          post = "/hr",
                                                          value = 15),
                                              sliderInput("assisthours2",
                                                          label=NULL,
                                                          min = 1,
                                                          max = 40,
                                                          post = " hrs",
                                                          value = 20),
                                              sliderInput("assistweeks2",
                                                          label=NULL,
                                                          min = 1,
                                                          max = 52,
                                                          post = " wks",
                                                          value = 30)),
                             # **Substitute Teacher -------------------------------------------------------
                             hr(),
                             h4("Substitute Teacher"),
                             p("This Indicates The Hours of Substitute Coverage for Teacher PD and Paid Time-Off"),
                             sliderInput("subhours1",
                                         label=NULL,
                                         min = 265,
                                         max = 420,
                                         step = 155,
                                         ticks = FALSE,
                                         post = " hrs",
                                         value = 265),
                             sliderInput("subrate1",
                                         label=NULL,
                                         min = 15,
                                         max = 25,
                                         step = .25,
                                         pre="$",
                                         post="/hr",
                                         value = 20),
                             # **Summer Teacher ------------------------------------------------------- 
                             hr(),
                             h4("Summer Teacher"),
                             sliderInput("sumsubrate1",
                                         label=NULL,
                                         min = 15,
                                         max = 25,
                                         step = .1,
                                         pre = "$",
                                         post = "/hr",
                                         value = 20),
                             sliderInput("sumsubhours1",
                                         label=NULL,
                                         min = 0,
                                         max = 400,
                                         post = " hrs",
                                         value = 200),
                             # **Classroom Supplies -------------------------------------------------------
                             hr(),
                             h4("Classroom Supplies"),
                             sliderInput("supplies1",
                                         label = NULL,
                                         min = 2500,
                                         max = 5500,
                                         step = 500,
                                         pre = "$",
                                         value = 3500),
                             # **Food -------------------------------------------------------
                             hr(),
                             h4("Food @ $2/Day Per Child"),
                             # **Enrollment -------------------------------------------------------
                             hr(),
                             h4("Enrollment"),
                             p("Taken From Your Elected CCS & Full-Pay Enrollment on The Revenues Tab"),
                             # **Overhead -------------------------------------------------------
                             hr(),
                             h4("Overhead Costs"),
                             sliderInput("overhead1",
                                         label=NULL,
                                         min = 20000,
                                         max = 40000,
                                         step = 5000,
                                         pre = "$",
                                         value = 30000),
                             # **Benefits + Fringe -------------------------------------------------------
                             hr(),
                             h4("Benefits/Fringe Costs"),
                             p("For FTs @ 25% of All FT Employee Salaries"),
                             sliderInput("fringerate",
                                         label=NULL,
                                         min = 0,
                                         max = 42,
                                         step = 1,
                                         post = "%",
                                         value = 25)))),
            
            # Show a plot of the generated distribution
            mainPanel(
                downloadButton('downloadggplot', "Download This Model"),
                br(),
                hr(),
                plotOutput("mod1Plot", width = "100%", height = "800px")
                
            )
        )
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    options(shiny.usecairo=T)
    options(shiny.launch.browser = TRUE)
    
    # FUNCTION: Model 1 -------------------------------------------------------
    
    output$mod1Plot <- renderPlot({
        # generate bins based on input$bins from ui.R

# **Condition: 1 Assistant/CDC ------------------------------------------------

        if (input$assistantselect == "1" & input$modeltype == "CDC-Based Employee") {

        mod1 <- tibble(
            Expenses = c(                                                                  #EXPENSES
                ((input$leadrate1*input$leadhours1)*input$leadweeks1)+                     #Teacher Annual Salary [(Rate*Hour)*Weeks]
                    ((input$assistrate1*input$assisthours1)*input$assistweeks1)+               #Assistant 1 Annual Salary [(Rate*Hour)*Weeks]
                    (input$subrate1*input$subhours1)+                                                     #Substitute Teacher Wages[Rate*265 Hours]
                    (input$sumsubrate1*input$sumsubhours1)+                                   #Summer Substitute Wages [Rate*Hours]
                    input$supplies1+                                                         #Supplies Costs [2500-5500]
                    input$overhead1+                                                         #Overhead Costs [20K-40K]
                    ((input$enrollccs+input$enrollfullpay)*2)+                                                      #Food Costs [CCSEnrollment*2] (From Revenues)
                    (((input$leadrate1*input$leadhours1)*input$leadweeks1)*(input$fringerate/100))),         #Fringe Benefits for Assistant 2
            #REVENUES
            Revenue = c((input$enrollpct/100)*                                             #Maximum Enrollment Percentage
                            ((input$enrollfullpay*input$tuitionrate)+                      #Full-Pay Student Revenue [FullPayEnroll*TuitionRate]
                                 (input$enrollccs*8872)+((.9*input$enrollccs)*3000)        #CCS-Pay Student Revenue [(CCSEnroll*TuitionRate)+((.9*CCSEnroll)*3000)]
                            )))
        #enrollfullpay tuititionrate enrollccs
        mod1 <- mod1 %>% gather(variable, value, 1:2)
        
        # draw the histogram with the specified number of bins
        
        gg_mod1 <- mod1 %>% ggplot() +
            geom_bar(aes(x = variable, y = value, fill = variable), stat= "identity",alpha = 0.95) +
            geom_text(aes(x=variable, y=value, label=scales::dollar(value, accuracy=1)), nudge_y = -22000, size = 8, fontface="bold", family = "roboto") +
            theme_ipsum_ps() +
            theme(plot.title = element_text(face="bold", size = 42, family = "roboto", margin = margin(b = 12)),
                  plot.subtitle = element_text(family = "roboto", size = 18, margin = margin(b = 10), lineheight=1),
                  plot.caption = element_text(family = "roboto", size = 7, color = "gray68"),
                  axis.text.x = element_text(family = "roboto", size = 16, color = "gray30"),
                  axis.text.y = element_text(family = "roboto", size = 17, color = "gray30"),
                  text=element_text(family="roboto")) +
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format(scale=.001, suffix = "k")) +
            guides(fill=FALSE,color=FALSE)+
            scale_fill_manual(values = c("#ff2700", "#008fd5")) +
            labs(title= paste(input$centername),
                 y=NULL,
                 x=NULL,
                 subtitle = paste("Scenario: ",input$modeltype,"\n",
                                  "\n",
                                  "REVENUES:\n",
                                  "Full-Pay Students Enrolled: ",input$enrollfullpay, "\n",
                                  "Tuition Rate: $",input$tuitionrate, "/seat\n",
                                  "CCS Students Enrolled: ",input$enrollccs, " @ $8872/student\n",
                                  "Percent of Pre-K Enrollment: ",input$enrollpct, "%\n",
                                  "\n",
                                  "EXPENSES:\n",
                                  "Pre-K Teacher: $",input$leadrate1, collapse=NULL,sep="","/hr | ",input$leadhours1,"hrs/wk | ",input$leadweeks1,"wks/yr\n",
                                  "Assistant Teacher: $",input$assistrate1,"/hr | ",input$assisthours1,"hrs/wk | ",input$assistweeks1,"wks/yr\n",
                                  "Substitute Teacher: $",input$subrate1, "/hr @ ",input$subhours1," hrs\n",
                                  "Summer Teacher: $",input$sumsubrate1,"/hr  @ ",input$sumsubhours1," hrs\n",
                                  "Classroom Supplies: $",input$supplies1,"\n",
                                  "Food Costs: $2/Day Per Child | ",(input$enrollccs+input$enrollfullpay)," Total Students\n",
                                  "Overhead: $",input$overhead1, "\n",
                                  "Benefits + Fringe: ",input$fringerate, "% for 1 FTE"))
            ggsave("plot.pdf", gg_mod1, device = "pdf")
            
            gg_mod1

# **Condition: 2 Assistants/CDC----------------------------------------------
            
        } else if (input$assistantselect == "2" & input$modeltype == "CDC-Based Employee") {
            
            mod2 <- tibble(
                Expenses = c(                                                                  #EXPENSES
                    ((input$leadrate1*input$leadhours1)*input$leadweeks1)+                     #Teacher Annual Salary [(Rate*Hour)*Weeks]
                    ((input$assistrate1*input$assisthours1)*input$assistweeks1)+               #Assistant 1 Annual Salary [(Rate*Hour)*Weeks]
                    ((input$assistrate2*input$assisthours2)*input$assistweeks2)+               #Assistant 2 Annual Salary [(Rate*Hour)*Weeks]
                     (input$subrate1*input$subhours1)+                                                     #Substitute Teacher Wages[Rate*265 Hours]
                     (input$sumsubrate1*input$sumsubhours1)+                                   #Summer Substitute Wages [Rate*Hours]
                      input$supplies1+                                                         #Supplies Costs [2500-5500]
                      input$overhead1+                                                         #Overhead Costs [20K-40K]
                     ((input$enrollccs+input$enrollfullpay)*2)+                                                      #Food Costs [CCSEnrollment*2] (From Revenues)
                   (((input$leadrate1*input$leadhours1)*input$leadweeks1)*(input$fringerate/100))),         #Fringe Benefits for Assistant 2
                #REVENUES
                Revenue = c((input$enrollpct/100)*                                             #Maximum Enrollment Percentage
                                ((input$enrollfullpay*input$tuitionrate)+                      #Full-Pay Student Revenue [FullPayEnroll*TuitionRate]
                                     (input$enrollccs*8872)+((.9*input$enrollccs)*3000)        #CCS-Pay Student Revenue [(CCSEnroll*TuitionRate)+((.9*CCSEnroll)*3000)]
                                ))) 
            #enrollfullpay tuititionrate enrollccs
            mod2 <- mod2 %>% gather(variable, value, 1:2)
            
            # draw the histogram with the specified number of bins
            
           gg_mod2 <- mod2 %>% ggplot() +
                geom_bar(aes(x=variable, y = value, fill = variable), stat= "identity",alpha = 0.95) +
                geom_text(aes(x=variable, y=value, label=scales::dollar(value, accuracy=1)), nudge_y = -22000, size = 8, fontface="bold", family = "roboto") +
                theme_ipsum_ps() +
                theme(plot.title = element_text(face="bold", size = 42, family = "roboto", margin = margin(b = 12)),
                      plot.subtitle = element_text(family = "roboto", size = 20, margin = margin(b = 10), lineheight=1),
                      plot.caption = element_text(family = "roboto", size = 7, color = "gray68"),
                      axis.text.x = element_text(family = "roboto", size = 16, color = "gray30"),
                      # axis.text.x = element_blank(),
                      axis.text.y = element_text(family = "roboto", size = 17, color = "gray30"),
                      text=element_text(family="roboto")) +
                # theme(plot.title = element_text(face="bold", size = 42, family = "Roboto-Black", margin = margin(b = 5)),
                #       plot.subtitle = element_text(family = "Roboto-Regular", size = 20, margin = margin(b = 10)),
                #       plot.caption = element_text(family = "Roboto-Regular", size = 7, color = "gray68"),
                #       axis.text.x = element_text(family = "Roboto-Regular", size = 12, color = "gray30"),
                #       axis.text.y = element_text(family = "Roboto-Regular", size = 15, color = "gray30"),
                #       text=element_text(family="Roboto")) +
                coord_flip() +
                scale_y_continuous(labels=scales::dollar_format(scale=.001, suffix = "k")) +
                guides(fill=FALSE,color=FALSE)+
                scale_fill_manual(values = c("#ff2700", "#008fd5")) +
                labs(title= paste(input$centername),
                     y=NULL,
                     x=NULL,
                     # fill = NULL,
                     subtitle = paste("Scenario: ",input$modeltype,"\n",
                                      "\n",
                                      "REVENUES:\n",
                                      "Full-Pay Students Enrolled: ",input$enrollfullpay, "\n",
                                      "Tuition Rate: $",input$tuitionrate, "/seat\n",
                                      "CCS Students Enrolled: ",input$enrollccs, " @ $8872/student\n",
                                      "Percent of Pre-K Enrollment: ",input$enrollpct, "%\n",
                                      "\n",
                                      "EXPENSES:\n",
                                      "Pre-K Teacher: $",input$leadrate1, collapse=NULL,sep="","/hr | ",input$leadhours1,"hrs/wk | ",input$leadweeks1,"wks/yr\n",
                                      "Asst. Teacher 1: $",input$assistrate1,"/hr | ",input$assisthours1,"hrs/wk | ",input$assistweeks1,"wks/yr\n",
                                      "Asst. Teacher 2: $",input$assistrate2,"/hr | ",input$assisthours2,"hrs/wk | ",input$assistweeks2,"wks/yr\n",
                                      "Substitute Teacher: $",input$subrate1, "/hr @ ",input$subhours1," hrs\n",
                                      "Summer Teacher: $",input$sumsubrate1, "/hr  @ ",input$sumsubhours1," hrs\n",
                                      "Classroom Supplies: $",input$supplies1, "\n",
                                      "Food Costs: $2/Day Per Child | ",(input$enrollccs+input$enrollfullpay)," Total Students\n",
                                      "Overhead: $",input$overhead1, "\n",
                                      "Benefits + Fringe: ",input$fringerate, "% for 1 FTE"))
           
                ggsave("plot.pdf", gg_mod2, device = "pdf")
                
                gg_mod2
                

# **Condition: 1 Assistant/UWATX----------------------------------------------

        } else if (input$assistantselect == "1" & input$modeltype == "UWATX-Based Employee") {

            mod3 <- tibble(
                Expenses = c(                                                                                   #---EXPENSES---E
                        ((input$leadrate2*input$leadhours1)*input$leadweeks1)+                                  # Pre-K Teacher Salary
                        ((input$assistrate1*input$assisthours1)*input$assistweeks1)+                            #Assistant Annual Salary [(Rate*Hour)*Weeks]
                        (input$subrate1*input$subhours1)+                                                       #Substitute Teacher Wages[Rate*265 Hours]
                        (input$sumsubrate1*input$sumsubhours1)+                                                 #Summer Substitute Wages [Rate*Hours]
                         input$supplies1+                                                                       #Supplies Costs [2500-5500]
                         input$overhead1+                                                                       #Overhead Costs [20K-40K]
                        ((input$enrollccs+input$enrollfullpay)*2)+                                              #Food Costs [CCSEnrollment*2] (From Revenues)
                        (((input$leadrate2*input$leadhours1)*input$leadweeks1)*(input$fringerate/100))),  #Fringe Benefits for Assistant
                                                                                                                #---REVENUES---#
                Revenue = c((input$enrollpct/100)*                                                              #Maximum Enrollment Percentage
                           ((input$enrollfullpay*input$tuitionrate)+                                            #Full-Pay Student Revenue [FullPayEnroll*TuitionRate]
                            (input$enrollccs*8872)+((.9*input$enrollccs)*3000))),                               #CCS-Pay Student Revenue [(CCSEnroll*TuitionRate)+((.9*CCSEnroll)*3000)]
                                                                                                                #---UWATX CONTRACT DOLLARS ---#              
                Contract = c(((input$leadrate2*input$leadhours1)*input$leadweeks1)+
                            (((input$leadrate2*input$leadhours1)*input$leadweeks1)*(input$fringerate/100)))) 
            #enrollfullpay tuititionrate enrollccs
            mod3 <- mod3 %>% gather(variable, value, 1:2)
            
            # draw the histogram with the specified number of bins
            
            gg_mod3 <- mod3 %>% ggplot() +
                geom_bar(aes(x=variable, y=value, fill = variable), stat= "identity",alpha = 0.95) +
                geom_text(aes(x=variable, y=value, label=scales::dollar((value-Contract), accuracy=1)), nudge_y = -22000, size = 8, fontface="bold", family = "roboto") +
                annotate("rect", fill = "white", xmin = 1.55, xmax = 2.45, ymin = 0, ymax = mod3$Contract, alpha = .5) +
                geom_text(aes(x=2.15, y=mod3$Contract), label="UWATX Managed\nRevenue", nudge_y = -(mod3$Contract/2), size = 7, family = "roboto") +
                geom_text(aes(x=1.8, y=mod3$Contract, label=scales::dollar(mod3$Contract, accuracy=1)), nudge_y = -(mod3$Contract/2), size = 8, fontface="bold", family = "roboto") +
                annotate("rect", fill = "white", xmin = 0.55, xmax = 1.45, ymin = 0, ymax = mod3$Contract, alpha = .5) + 
                geom_text(aes(x=1.15, y=mod3$Contract), label="UWATX Managed\nExpenses", nudge_y = -(mod3$Contract/2), size = 7, family = "roboto") +
                geom_text(aes(x=.8, y=mod3$Contract, label=scales::dollar(mod3$Contract, accuracy=1)), nudge_y = -(mod3$Contract/2), size = 8, fontface="bold", family = "roboto") +
                theme_ipsum_ps() +
                theme(plot.title = element_text(face="bold", size = 42, family = "roboto", margin = margin(b = 12)),
                      plot.subtitle = element_text(family = "roboto", size = 18, margin = margin(b = 10), lineheight=1),
                      plot.caption = element_text(family = "roboto", size = 7, color = "gray68"),
                      axis.text.x = element_text(family = "roboto", size = 16, color = "gray30"),
                      axis.text.y = element_text(family = "roboto", size = 17, color = "gray30"),
                      text=element_text(family="roboto")) +
                coord_flip() +
                scale_y_continuous(labels=scales::dollar_format(scale=.001, suffix = "k")) +
                guides(fill=FALSE,color=FALSE)+
                scale_fill_manual(values = c("#ff2700", "#008fd5")) +
                labs(title= paste(input$centername),
                     y=NULL,
                     x=NULL,
                     subtitle = paste("Scenario: ",input$modeltype,"\n",
                                      "\n",
                                      "REVENUES:\n",
                                      "Full-Pay Students Enrolled: ",input$enrollfullpay, "\n",
                                      "Tuition Rate: $",input$tuitionrate, "/seat\n",
                                      "CCS Students Enrolled: ",input$enrollccs, " @ $8872/student\n",
                                      "Percent of Pre-K Enrollment: ",input$enrollpct, "%\n",
                                      "\n",
                                      "EXPENSES:\n",
                                      "Pre-K Teacher: $",input$leadrate2, collapse=NULL,sep="","/hr | ",input$leadhours1,"hrs/wk | ",input$leadweeks1,"wks/yr\n",
                                      "Assistant Teacher: $",input$assistrate1,"/hr | ",input$assisthours1,"hrs/wk | ",input$assistweeks1,"wks/yr\n",
                                      "Substitute Teacher: $",input$subrate1, "/hr @ ",input$subhours1," hrs\n",
                                      "Summer Teacher: $",input$sumsubrate1,"/hr  @ ",input$sumsubhours1," hrs\n",
                                      "Classroom Supplies: $",input$supplies1,"\n",
                                      "Food Costs: $2/Day Per Child | ",(input$enrollccs+input$enrollfullpay)," Total Students\n",
                                      "Overhead: $",input$overhead1, "\n",
                                      "Benefits + Fringe: ",input$fringerate, "% for 1 FTE"))
            ggsave("plot.pdf", gg_mod3, device = "pdf")
            
            gg_mod3 
            
# **Condition: 2 Assistants/UWATX----------------------------------------------
        } else {
            
            mod4 <- tibble(
                Expenses = c(                                                                                      #---EXPENSES---#
                    ((input$leadrate1*input$leadhours1)*input$leadweeks1)+                                         #Teacher Annual Salary [(Rate*Hour)*Weeks]
                        ((input$assistrate1*input$assisthours1)*input$assistweeks1)+                               #Assistant 1 Annual Salary [(Rate*Hour)*Weeks]
                        ((input$assistrate2*input$assisthours2)*input$assistweeks2)+                               #Assistant 2 Annual Salary [(Rate*Hour)*Weeks]
                        (input$subrate1*input$subhours1)+                                                          #Substitute Teacher Wages[Rate*265 Hours]
                        (input$sumsubrate1*input$sumsubhours1)+                                                    #Summer Substitute Wages [Rate*Hours]
                        input$supplies1+                                                                           #Supplies Costs [2500-5500]
                        input$overhead1+                                                                           #Overhead Costs [20K-40K]
                        ((input$enrollccs+input$enrollfullpay)*2)+                                                 #Food Costs [CCSEnrollment*2] (From Revenues)
                        (((input$leadrate1*input$leadhours1)*input$leadweeks1)*(input$fringerate/100))),     #Fringe Benefits for Assistant 2
                                                                                                                   #---REVENUES---#
                Revenue = c((input$enrollpct/100)*                                                                 #Maximum Enrollment Percentage
                                ((input$enrollfullpay*input$tuitionrate)+                                          #Full-Pay Student Revenue [FullPayEnroll*TuitionRate]
                                     (input$enrollccs*8872)+((.9*input$enrollccs)*3000))),                         #CCS-Pay Student Revenue [(CCSEnroll*TuitionRate)+((.9*CCSEnroll)*3000)]
                                                                                                                   #---UWATX CONTRACT DOLLARS ---#              
                Contract = c(((input$leadrate2*input$leadhours1)*input$leadweeks1)+
                                 (((input$leadrate2*input$leadhours1)*input$leadweeks1)*(input$fringerate/100))))
            #enrollfullpay tuititionrate enrollccs
            mod4 <- mod4 %>% gather(variable, value, 1:2)
            
            # draw the histogram with the specified number of bins
            
            gg_mod4 <- mod4 %>% ggplot() +
                geom_bar(aes(x = variable, y = value, fill = variable), stat= "identity",alpha = 0.95) +
                geom_text(aes(x=variable, y=value, label=scales::dollar((value-Contract), accuracy=1)), nudge_y = -22000, size = 8, fontface="bold", family = "roboto") +
                annotate("rect", fill = "white", xmin = 1.55, xmax = 2.45, ymin = 0, ymax = mod4$Contract, alpha = .5) +
                geom_text(aes(x=2.15, y=mod4$Contract), label="UWATX Managed\nRevenue", nudge_y = -(mod4$Contract/2), size = 7, family = "roboto") +
                geom_text(aes(x=1.8, y=mod4$Contract, label=scales::dollar(mod4$Contract, accuracy=1)), nudge_y = -(mod4$Contract/2), size = 8, fontface="bold", family = "roboto") +
                annotate("rect", fill = "white", xmin = 0.55, xmax = 1.45, ymin = 0, ymax = mod4$Contract, alpha = .5) + 
                geom_text(aes(x=1.15, y=mod4$Contract), label="UWATX Managed\nExpenses", nudge_y = -(mod4$Contract/2), size = 7, family = "roboto") +
                geom_text(aes(x=.8, y=mod4$Contract, label=scales::dollar(mod4$Contract, accuracy=1)), nudge_y = -(mod4$Contract/2), size = 8, fontface="bold", family = "roboto") +
                theme_ipsum_ps() +
                theme(plot.title = element_text(face="bold", size = 42, family = "roboto", margin = margin(b = 12)),
                      plot.subtitle = element_text(family = "roboto", size = 18, margin = margin(b = 10), lineheight=1),
                      plot.caption = element_text(family = "roboto", size = 7, color = "gray68"),
                      axis.text.x = element_text(family = "roboto", size = 16, color = "gray30"),
                      axis.text.y = element_text(family = "roboto", size = 17, color = "gray30"),
                      text=element_text(family="roboto")) +
                coord_flip() +
                scale_y_continuous(labels=scales::dollar_format(scale=.001, suffix = "k")) +
                guides(fill=FALSE,color=FALSE)+
                scale_fill_manual(values = c("#ff2700", "#008fd5")) +
                labs(title= paste(input$centername),
                     y=NULL,
                     x=NULL,
                     subtitle = paste("Scenario: ",input$modeltype,"\n",
                                      "\n",
                                      "REVENUES:\n",
                                      "Full-Pay Students Enrolled: ",input$enrollfullpay, "\n",
                                      "Tuition Rate: $",input$tuitionrate, "/seat\n",
                                      "CCS Students Enrolled: ",input$enrollccs, " @ $8872/student\n",
                                      "Percent of Pre-K Enrollment: ",input$enrollpct, "%\n",
                                      "\n",
                                      "EXPENSES:\n",
                                      "Pre-K Teacher: $",input$leadrate2, collapse=NULL,sep="","/hr | ",input$leadhours1,"hrs/wk | ",input$leadweeks1,"wks/yr\n",
                                      "Asst. Teacher 1: $",input$assistrate1,"/hr | ",input$assisthours1,"hrs/wk | ",input$assistweeks1,"wks/yr\n",
                                      "Asst. Teacher 2: $",input$assistrate2,"/hr | ",input$assisthours2,"hrs/wk | ",input$assistweeks2,"wks/yr\n",
                                      "Substitute Teacher: $",input$subrate1, "/hr @ ",input$subhours1," hrs\n",
                                      "Summer Teacher: $",input$sumsubrate1,"/hr  @ ",input$sumsubhours1," hrs\n",
                                      "Classroom Supplies: $",input$supplies1,"\n",
                                      "Food Costs: $2/Day Per Child | ",(input$enrollccs+input$enrollfullpay)," Total Students\n",
                                      "Overhead: $",input$overhead1, "\n",
                                      "Benefits + Fringe: ",input$fringerate, "% for 1 FTE"))
            ggsave("plot.pdf", gg_mod4, device = "pdf")
            
            gg_mod4
            
        }  
    })
    
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
