# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

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
                    way you solve a problem (ex.At most vs At least)")
          ),
          br(), 
          h3("Counting Techniques"),
          fluidRow(
            box(
              withMathJax(), 
              title = strong("Combinations"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 6,
              tags$ul(
                tags$li("Combinations of ", tags$em("n"), "objects taken ", 
                        tags$em("r"), "at a time"), 
                tags$li("Number of ", tags$em("unordered"), "subsets")
              )
            ),
            box(
              title = strong("Permutations"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 6,
              p("If there are:"),
              tags$ul( 
                tags$li(tags$em("n"), "choices for the 1st position"), 
                tags$li(tags$em("n-1"), "choices for the 2nd position"), 
                tags$li(tags$em("n-2"), "choices for the 3rd position"), 
                tags$li("Then nPr or permutations of ", tags$em("n"), "objects
                        taken", tags$em("n"), "at a time = "), 
                tags$li("n * (n - 1) * (n - 2) * ... * 1 = n!")
              )
            )
          ),
          
          fluidRow(
            box(
              title = strong("Distinguishable Permutations"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 6,
              p("Number of distinguishable permutations of ", tags$em("n"), "objects:"),
              tags$ul( 
                tags$li(tags$em("n1"), "of object one"), 
                tags$li(tags$em("n2"), "of object two"), 
                tags$li(tags$em("nk"), "of object k"), 
                tags$li("Then nPr or permutations of ", tags$em("n"), "objects
                        taken", tags$em("n"), "at a time = "), 
                tags$li("n * (n - 1) * (n - 2) * ... * 1 = n!")
              )
            ),
            box(
              title = strong("Multiplication Principle"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 6,
              p("If there are: "),
              tags$ul( 
                tags$li(tags$em("n1"), "outcomes of experiment one"), 
                tags$li(tags$em("n2"), "outcomes of experiment two"), 
                tags$li(tags$em("nm"), "outcomes of experiment m"), 
                tags$li("Then the number of total outcomes from all experiments", 
                        tags$em("E1, E2,... Em"), "="), 
                tags$li("n1 * n2 * n3 * ... * nm")
              )
            )
          ),
          br(), 
          h3("Binomial Probability: Exactly vs. At Most vs. At Least"), 
          fluidRow(
            box(
              title = strong("Exactly"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 4,
              "In the Confirmatory Data Analysis tradition, null hypothesis
              significance tests serve as a critical tool to confirm that a
              particular theoretical model describes our data and to make a
              generalization from our sample to the broader population
              (i.e., make an inference). The null hypothesis often reflects the
              simpler of two models (e.g., 'no statistical difference',
              'there is an additive difference of 1', etc.) that we will use to
              build a sampling distribution for our chosen estimator. These
              methods let us test whether our sample data are consistent with this
              simple model (null hypothesis)."
            ),
            box(
              title = strong("At Most"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 4,
              "The probability that our selected estimator takes on a value at
              least as extreme as what we observed given our null hypothesis. If
              we were to carry out our study infinitely many times and the null
              hypothesis accurately modeled what we're studying, then we would
              expect for our estimator to produce a value at least as extreme as
              what we have seen 100*(p-value)% of the time. The larger the
              p-value, the more often we would expect our estimator to take on a
              value at least as extreme as what we've seen; the smaller, the less
              often."
            ), 
            box(
              title = strong("At Least"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 4,
              "The probability that our selected estimator takes on a value at
              least as extreme as what we observed given our null hypothesis. If
              we were to carry out our study infinitely many times and the null
              hypothesis accurately modeled what we're studying, then we would
              expect for our estimator to produce a value at least as extreme as
              what we have seen 100*(p-value)% of the time. The larger the
              p-value, the more often we would expect our estimator to take on a
              value at least as extreme as what we've seen; the smaller, the less
              often."
            )
          ),
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
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore Page
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the Concept"),
          p("This page should include something for the user to do, the more
            active and engaging, the better. The purpose of this page is to help
            the user build a productive understanding of the concept your app
            is dedicated to."),
          p("Common elements include graphs, sliders, buttons, etc."),
          p("The following comes from the NHST Caveats App:"),
          
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
        text = "This App Template will help you get started building your own app"
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
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
