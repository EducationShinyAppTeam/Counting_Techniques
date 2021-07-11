# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(shinyjs)

# Load additional dependencies and setup functions
# source("global.R")


# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Counting Techniques", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "App_Template")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Counting Techniques"), # This should be the full name.
          p("This app is designed to teach and review combinatorics, permutations
            , and other basic counting principles."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review the Prerequistes tab to learn about the different
                    counting techniques as well as how to use context and/or wording
                    fromthe problem to solve it."),
            tags$li("Use the Explore page to practice counting techniques through 
                    clothing-related scenarios."),
            tags$li("Use the Challenge page to further practice counting techniques
                    through poker-style scenarios.")
          ),
          
          ##### Go Button--location will depend on your goals ----
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
          ##### Create two lines of space ----
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Kellien Peritz and 
            Shravani Samala.",
            br(),
            "We would like to extend a special thanks to Dr. Dennis Pearl for providing 
            the question bank.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/7/2021 by NJH.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("The difference between combinations, and the other counting 
                    techniques."),
            tags$li("Understand how different wording in a problem changes the
                    way you solve a problem (e.g., at most vs. at least)")
          ),
          br(), 
          h3("Counting Techniques"),
          fluidRow(
            box(
              title = strong("Combinations"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 6,
              p("If there are:"),
              tags$ul(
                tags$li("\\({n}\\) objects to be taken \\({r}\\) at a time"), 
                tags$li("Number of ", tags$em("unordered"), "subsets or Combinations is "), 
                tags$li("\\(_{n}C_{r}=\\dfrac{n!}{r!(n-r)!}\\)")
              )
            ),
            box(
              title = strong("Permutations"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 6,
              p("If there are:"),
              tags$ul( 
                tags$li("\\({n}\\) choices for the \\(1^{st}\\) position"), 
                tags$li("\\({n-1}\\) choices for the \\(2^{nd}\\) position"), 
                tags$li("\\({n-2}\\) choices for the \\(3^{rd}\\) position"), 
                tags$li("Then nPr or permutations of ", tags$em("n"), "objects
                        that can be arranged in ", ("r"), "at a time or ordered 
                        subsets is "), 
                tags$li("\\(_{n}P_{r}=\\dfrac{n!}{(n-r)!}\\)")
              )
            )
          ),
          
          fluidRow(
            box(
              title = strong("Distinguishable Permutations"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 6,
              p("Number of distinguishable permutations of ", tags$em("n"), "objects:"),
              tags$ul( 
                tags$li("\\(n_{1}\\) of the \\(1^{st}\\) object."),
                tags$li("\\(n_{2}\\) of the \\(2^{nd}\\) object."),
                tags$li("\\(n_{k}\\) of the \\(k^{th}\\) object."),
                tags$li("Then nPr or permutations of ", tags$em("n"), "objects
                        taken", tags$em("n"), "at a time is "),
                tags$li("\\(P=\\binom{n}{(n_{1})(n_{2})\\cdots(n_{k})}=\\dfrac{n!}{(n_{1})!(n_{2})!(n_{3})!\\cdots(n_{k})!}\\)")
                ### ask how to line up the equal signs for the n!/n
              )
            ),
            box(
              title = strong("Multiplication Principle"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 6,
              p("If there are: "),
              tags$ul( 
                tags$li(("\\({n_{1}}\\)"), "outcomes of experiment one"), 
                tags$li(("\\({n_{2}}\\)"), "outcomes of experiment two"), 
                tags$li(("\\({n_{m}}\\)"), "outcomes of experiment m"), 
                tags$li("Then the number of total outcomes from all experiments", 
                        ("\\({E_{1},E_{2},E_{3}=}\\)")), 
                tags$li("\\({(n_{1})\\times(n_{2})\\times(n_{3})\\times\\cdots\\times(n_{m})= n!}\\)")
              )
            )
          ),
        
          br(), 
          
          div(
            style = "text-align: center",
            bsButton(
              inputId = "goPre",
              label = "GO!",
              size = "large",
              icon = icon("wpexplorer"),
              style = "default"
            )
          )
        ),
        
        
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the Concept"),
          tabsetPanel(
            tabPanel(
              withMathJax(),
              title = "Multiple Choice Layout", 
              br(), 
                h4("Question"),
                uiOutput("context"), 
                uiOutput("question"),
                br(),
                fluidRow(
                  column(
                  width = 12, 
                    bsButton(
                      inputId = "hint",
                      label = "Hint",
                      icon = icon("question"),
                      size = "large",
                      disabled = FALSE
                    ),
                    br(), 
                    uiOutput("hintDisplay"),
                    br(),
                  )
                ), 
                fluidRow(
                  column(width = 8, 
                    radioGroupButtons(
                      inputId = "mc1",
                      label = tags$b("Which expression addresses the question?"),
                      status = "game",
                      direction = "vertical",
                      selected = character(0),
                      checkIcon = list(
                        yes = icon("check-square"),
                        no = icon("square-o")
                      ),
                     
                      choices = list(
                        # "Pick the expression below that best addresses the question.",
                        "\\(\\frac{1}{4}\\)",
                        "\\(\\frac{2}{4}\\)",
                        "\\(\\frac{3}{4}\\)",
                        "\\(\\frac{4}{4}\\)"
                      ),
                      justified = FALSE,
                      individual = FALSE, 
                    ),  
                    br(), 
                  )
                ), 
                fluidRow(
                  column(
                    width = 2, 
                    bsButton(
                      "restart",
                      "Restart",
                      size = "large",
                      style = "danger",
                      disabled = FALSE
                    ), 
                  ), 
                  column(
                    width = 2, 
                    bsButton(
                      inputId = "nextq",
                      label = "Next Question",
                      size = "large",
                      style = "default",
                      disabled = TRUE
                    ),
                  ), 
                  column(
                    width = 2, 
                    bsButton(
                      inputId = "submit",
                      label = "Submit",
                      size = "large",
                      style = "default",
                      disabled = FALSE
                    ), 
                    
                  ),
                  column(
                    width = 2, 
                    uiOutput("mark")
                  )
                ),
              fluidRow(
                column(
                  width = 12, 
                  br(), 
                  uiOutput("feedback")
                )
                
              ), 
              uiOutput("math1"),
              uiOutput("math2")
              ) 
            
            ), 
          br(), 
          br(),
          br(), 
          
          div(
            style = "text-align: center",
            bsButton(
              inputId = "goExp",
              label = "GO!",
              size = "large",
              icon = icon("gamepad"),
              style = "default"
            )
          ) 
        ), 
  
       
        #### Set up a Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Practice/Test Yourself with [Type of Game]"),
          p("On this type of page, you'll set up a game for the user to play.
            Game types include Tic-Tac-Toe, Matching, and a version Hangman to
            name a few. If you have ideas for new game type, please let us know.")
        ),
        
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. 
            R package version 0.1.6.3. Available from 
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. 
            (2020). shiny: Web Application Framework for R. R package version 
            1.5.0. Available from https://CRAN.R-project.org/package=shiny"
          ),
          
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard: 
            Create Dashboards with 'Shiny'. R package version 0.7.1. Available 
            from https://CRAN.R-project.org/package=shinydashboard"
          ),
          
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets: 
            Custom Inputs Widgets for Shiny. R package version 0.5.3. Available 
            from https://CRAN.R-project.org/package=shinyWidgets"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This app is designed to help you learn and review Counting
        Techniques"
      )
    }
  )
  
  observeEvent(input$go1,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "prerequisites")
  })
  
  observeEvent(input$goPre,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "explore")
  })
  
  observeEvent(input$goExp,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "game")
  })
  
  ###Explore Page Practice ----
  withBusyIndicatorServer <- function(buttonId, expr) {
    # UX stuff: show the "busy" message, hide the other messages, disable the button
    loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
    doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    shinyjs::disable(buttonId)
    shinyjs::show(selector = loadingEl)
    shinyjs::hide(selector = doneEl)
    shinyjs::hide(selector = errEl)
    on.exit({
      shinyjs::enable(buttonId)
      shinyjs::hide(selector = loadingEl)
    })

    # Try to run the code when the button is clicked and show an error message if
    # an error occurs or a success message if it completes
    tryCatch({
      value <- expr
      shinyjs::show(selector = doneEl)
      shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                         time = 0.5))
      value
    }, error = function(err) { errorFunc(err, buttonId) })
  }
  
  ##### Reading in Questions ----
  questionBank <- read.csv("exploreQuestions.csv", stringsAsFactors = FALSE)
  Qs_array <- c(1:nrow(questionBank))

  # Reset button
  observeEvent(input$restart, {
    withMathJax()
    updateButton(
      session = session, 
      inputId = "submit", 
      disabled = FALSE)
    updateButton(
      session = session, 
      inputId = "nextq", 
      disabled = FALSE)
    updateButton(
      session = session, 
      inputId = "restart", 
      disabled = FALSE)
    
    
    Qs <<- nrow(questionBank)
    Qs_array <<- c(1:Qs)
    id <- 1
    
    output$question <- renderUI({
      withMathJax()
      hint <<- withMathJax(questionBank[id, "Hint"])
      return(paste(questionBank[id, "Scenario"], questionBank[id, "Question"]))
    })
    
    output$hint <- renderUI({
      withMathJax()
      hint <<- withMathJax(questionBank[id, "Hint"])
      return(questionBank[id, "Hint"])
    })
    
    output$feedback <- renderUI({
      withMathJax()
      hint <<- withMathJax(questionBank[id, "Hint"])
      return(withMathJax(questionBank[id, "Feedback"]))
    })
    
    updateRadioGroupButtons(
      session, "mc1",
      choices = list(
        questionBank[id, "A"],
        questionBank[id, "B"],
        questionBank[id, "C"],
        questionBank[id, "D"] 
      ),
      selected = character(0),
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square-o")
      ),
    )
    output$math1 <- renderUI({
      withMathJax()
    })
    output$math2 <- renderUI({
      withMathJax()
    })
    output$mark <- renderUI({
      img(src = NULL, width = 50)
    })
    value[["mistake"]] <<- 0
    value$correct <<- 0
  })
  
  # Print out a question
  output$question <- renderUI({
    withMathJax()
    id <<- sample(Qs_array, 1, replace = FALSE, prob = NULL)
    Qs_array <<- Qs_array[!Qs_array %in% id]
    updateRadioGroupButtons(
      session, "mc1",
      selected = character(0),
      choices = list(
        questionBank[id, "A"],
        questionBank[id, "B"],
        questionBank[id, "C"],
        questionBank[id, "D"]
      ),
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square-o")
      ),
      status = "game"
    )
    output$math1 <- renderUI({
      withMathJax()
    })
    output$math2 <- renderUI({
      withMathJax()
    })
    hint <<- withMathJax(questionBank[id, "Hint"])
    return(withMathJax(paste(questionBank[id, "Scenario"], questionBank[id, "Question"])))
  })
  
  ### NEXT QUESTION BUTTON###
  observeEvent(input$nextq, {
    withMathJax()
    if (length(Qs_array) > 1) {
      id <<- sample(Qs_array, 1, replace = FALSE, prob = NULL)
      Qs_array <<- Qs_array[!Qs_array %in% id]
      hint <<- questionBank["Hint"]
      withBusyIndicatorServer("nextq", {
        updateButton(session, "submit", disabled = FALSE)
        output$question <- renderUI({
          return(paste(questionBank[id, "Scenario"], questionBank[id, "Question"]))
        })
        
        updateRadioGroupButtons(
          session, "mc1",
          selected = character(0),
          choices = list(
            questionBank[id, "A"],
            questionBank[id, "B"],
            questionBank[id, "C"],
            questionBank[id, "D"] 
          ),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "game"
        )
        output$math1 <- renderUI({
          withMathJax()
        })
        output$math2 <- renderUI({
          withMathJax()
        })
        output$mark <- renderUI({
          img(src = NULL, width = 50)
        })
      })
      
      ##HINT###
      output$hintDisplay <- renderUI({
        return(NULL)
      })
      output$feedback <- renderUI({
        return(NULL)
      })
    }
    else if (length(Qs_array) == 1) {
      id <<- Qs_array[1]
      Qs_array <<- Qs_array[!Qs_array %in% id]
      hint <<- questionBank[id, "Hint"]
      withBusyIndicatorServer("nextq", {
        output$question <- renderUI({
          return(paste(questionBank[id, "Scenario"], questionBank[id, "Question"]))
        })
        
        updateButton(
          session = session, 
          inputId = "submit", 
          disabled = FALSE)
        updateRadioGroupButtons(
          session = session, 
          inputId = "mc1",
          selected = character(0),
          choices = list(
            questionBank[id, "A"],
            questionBank[id, "B"],
            questionBank[id, "C"],
            questionBank[id, "D"] 
          ),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
        )
        output$math1 <- renderUI({
          withMathJax()
        })
        output$math2 <- renderUI({
          withMathJax()
        })
        output$mark <- renderUI({
          img(src = NULL, width = 50)
        })
      })
      
      ##HINT###
      output$hintDisplay <- renderUI({
        return(NULL)
      })
      output$feedback <- renderUI({
        return(NULL)
      })
    }
    else {
      updateButton(
        session = session, 
        inputId = "submit", 
        disabled = TRUE)
      updateButton(
        session = session, 
        inputId = "nextq", 
        disabled = TRUE)
      updateButton(
        session = session, 
        inputId = "restart", 
        disabled = FALSE)
      sendSweetAlert(
        session = session,
        title = "Run out of question",
        type = "error",
        closeOnClickOutside = TRUE,
        h4("Run out of question. Please click Restart to start over")
      )
      output$question <- renderUI({
        return(NULL)
      })
      
      output$hintDisplay <- renderUI({
        return(NULL)
      })
      
      output$feedback <- renderUI({
        return(NULL)
      })
      updateRadioGroupButtons(
        session, "mc1",
        selected = character(0),
        choices = list(
          questionBank[id, "A"],
          questionBank[id, "B"],
          questionBank[id, "C"],
          questionBank[id, "D"] 
        ),
        checkIcon = list(
          yes = icon("check-square"),
          no = icon("square-o")
        ),
        status = "game"
      )
      output$math1 <- renderUI({
        withMathJax()
      })
      output$math2 <- renderUI({
        withMathJax()
      })
    }
  })
  
  ### SUBMIT BUTTON###
  observeEvent(input$submit, {
    withMathJax()
    letterAnswer <- questionBank[id, "Answer"]
    cAnswer <- questionBank[id, letterAnswer]
    mc1Length <- length(input$mc1)
    print(mc1Length)
    print(letterAnswer)
    print(cAnswer)
    if(length(input$mc1) == 0){
        answer = "E"
        updateButton(
          session = session, 
          inputId = "submit", 
          disabled = TRUE)
        updateButton(
          session = session, 
          inputId = "nextq", 
          disabled = FALSE)
        updateButton(
          session = session, 
          inputId = "restart", 
          disabled = FALSE)
        
        output$mark <- renderIcon(
          icon = ifelse(
            test = answer == cAnswer, 
            yes = "correct", 
            no = "incorrect"
          )
        )
    }
    else{
      input$mc1 == input$mc1
      updateButton(
        session = session, 
        inputId = "submit", 
        disabled = TRUE)
      updateButton(
        session = session, 
        inputId = "nextq", 
        disabled = FALSE)
      updateButton(
        session = session, 
        inputId = "restart", 
        disabled = FALSE)
      
      output$mark <- renderIcon(
        icon = ifelse(
          test = input$mc1 == cAnswer, 
          yes = "correct", 
          no = "incorrect"
        )
      )
    }
  
    ### PRINT HINTS###
    observeEvent(
      eventExpr = input$hint, 
      handlerExpr = {
        output$math1 <- renderUI({
          withMathJax()
        })
        output$math2 <- renderUI({
          withMathJax()
        })
        withMathJax()
        output$hintDisplay <- renderUI({
          p(tags$b("Hint:"), questionBank[id, "Hint"])
        })
      })
      
    ### FEEDBACK###
    output$feedback <- renderUI({
      withMathJax()
      letterAnswer <- questionBank[id, "Answer"]
      cAnswer <- questionBank[id, letterAnswer]
      if(length(input$mc1) == 0){
        answer = "E"
      }
      else {
        answer = input$mc1
      }
      if (answer == cAnswer) {
        p("CORRECT!", br(), withMathJax(questionBank[id, "Feedback"]))
      }
      else if (answer == "E"){
        p(strong("Answer:"), br(), questionBank[id, "Answer"], 
        br(), strong("Explanation:"), br(),  withMathJax(questionBank[id, "Feedback"]))
      }
      else{
        p(strong("Answer:"), br(), questionBank[id, "Answer"], 
        br(), strong("Explanation:"), br(),  withMathJax(questionBank[id, "Feedback"]))
      }
    })
  })
}
# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)