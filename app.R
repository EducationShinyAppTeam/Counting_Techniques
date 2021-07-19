# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(shinyjs)
library(data.table)

# Load additional dependencies and setup functions

pokerHands <- fread("www/pokerquestionbank.csv")
cardBacks <- function(){
  return(img(src = "pokercard-back.png",
             width = "100%",
             contentType = "image/png", 
             alt = "Poker card backside"))
}

checker_shoes = img(src = "checker-shoes.jpg")
psu_adidas = img(src = "psu-adidas.jpg")
tshirt_solid = img(src = "tshirt-solid.jpg")
tshirt_stripe = img(src = "tshirt-stripe.jpg")
hoodie_solid = img(src = "hoodie-solid.jpg")
hoodie_stripe = img(src = "hoodie-stripe.jpg")
pants_solid = img(src = "pants-solid.jpg")
pants_stripe = img(src = "pants-stripe.jpg")
shorts_solid = img(src = "shorts-solid.jpg")
shorts_stripe = img(src = "shorts-stripe.jpg")

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
        #### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Counting Techniques"), 
          p("This app is designed to teach and review combinatorics and 
            its application in probability."),
          h2("Instructions"),
          tags$ol(
            tags$li("Use the Prerequistes page to review the different counting 
                    principles and the keywords to be aware of when solving problems."),
            tags$li("Use the Explore page to practice combinatorics in 
                    clothing-related problems."),
            tags$li("Use the Challenge page to practice applying combinatorics to 
                    probability in poker-related problems.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "overviewToPrereq",
              label = "Prerequisites!",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
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
        
        #### Prerequisites Page ----
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
              title = strong("Permutation with Replacement"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 6,
              p("Number of ways to pick r things from n possibilities:"),
              tags$ul( 
                tags$li("\\(n^{r}\\)"),
                tags$li("Ordered subsets with replacement")
                
              )
            ),
            box(
              title = strong("Permutation without Replacement"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 6,
              p("Number of ways to pick r things from n possibilities:"),
              tags$ul( 
                tags$li("\\(_{n}P_{r}=\\dfrac{n!}{(n-r)!}\\)"),
                tags$li("Ordered subsets without replacement")
              )
            )
          ),
          fluidRow(
            box(
              title = strong("Combination with Replacement"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 6,
              p("Number of ways to pick r things from n possibilities:"),
              tags$ul(
                tags$li("\\(\\binom{n+r-1}{r} = \\dfrac{(n+r-1)!}{r!(n-1)!}\\)"),
                tags$li("Unordered subsets with replacement")
              )
            ),
            box(
              title = strong("Combination without Replacement"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 6,
              p("Number of ways to pick r things from n possibilities:"),
              tags$ul(
                tags$li("\\(\\binom{n}{r} = \\dfrac{n!}{r!(n-r)!}\\)"),
                tags$li("Unordered subsets without replacement")
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
                tags$li("Then \\_{n}P_{r}\\) or permutations of ", tags$em("n"), "objects
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
            style = "text-align: center;",
            bsButton(
              inputId = "prereqToExplore",
              label = "Explore!",
              size = "large",
              icon = icon("wpexplorer"),
              style = "default"
            )
          )
        ),
        ####Explore page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the Concept"),
          tabsetPanel(
            ##### CLOTHING TAB ----
            tabPanel(
              title = "Practice Combinations and Permutations",
              br(),
              fluidRow(
                column(
                  width = 2,
                  align="center",
                  offset = 2,
                  tags$img(
                    src = "tshirt_solid.jpg",
                    alt = "Solid t-shirt",
                    width = "100%"
                  ),
                  textOutput("tshirt_solid_num")
                ),
                column(
                  width = 2,
                  align="center",
                  offset = 0,
                  tags$img(
                    src = "tshirt_stripe.jpg",
                    alt = "Striped t-shirt",
                    width = "100%"
                  ),
                  textOutput("tshirt_stripe_num")
                ),
                column(
                  width = 2,
                  align="center",
                  offset = 0,
                  tags$img(
                    src = "hoodie_solid.jpg",
                    alt = "Solid hoodie",
                    width = "100%"
                  ),
                  textOutput("hoodie_solid_num")
                ),
                column(
                  width = 2,
                  align="center",
                  offset = 0,
                  tags$img(
                    src = "hoodie_stripe.jpg",
                    alt = "Striped hoodie",
                    width = "100%"
                  ),
                  textOutput("hoodie_stripe_num")
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  align="center",
                  offset = 2,
                  tags$img(
                    src = "pants_solid.jpg",
                    alt = "Solid pants",
                    width = "100%"
                  ),
                  textOutput("pants_solid_num")
                ),
                column(
                  width = 2,
                  align="center",
                  offset = 0,
                  tags$img(
                    src = "pants_stripe.jpg",
                    alt = "Striped pants",
                    width = "100%"
                  ),
                  textOutput("pants_stripe_num")
                ),
                column(
                  width = 2,
                  align="center",
                  offset = 0,
                  tags$img(
                    src = "shorts_solid.jpg",
                    alt = "Solid shorts",
                    width = "100%"
                  ),
                  textOutput("shorts_solid_num")
                ),
                column(
                  width = 2,
                  align="center",
                  offset = 0,
                  tags$img(
                    src = "shorts_stripe.jpg",
                    alt = "Striped shorts",
                    width = "100%"
                  ),
                  textOutput("shorts_stripe_num")
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  align="center",
                  offset = 4,
                  tags$img(
                    src = "checker_shoes.jpg",
                    alt = "Checkered shoes",
                    width = "100%"
                  ),
                  textOutput("checker_shoes_num")
                ),
                column(
                  width = 2,
                  align="center",
                  offset = 0,
                  tags$img(
                    src = "psu_adidas.jpg",
                    alt = "Penn State Adidas",
                    width = "100%"
                  ),
                  textOutput("psu_adidas_num")
                )
              ),
              br(),
              fluidRow(
                div(
                  style = "text-align: center",
                  bsButton(
                    inputId = "newWardrobe",
                    label = "New Wardrobe",
                    size = "large",
                    style = "default"
                  )
                )
              ),
              br(),
              h4(tags$b("Practice Problems")),
              p("If you are struggling solving the following combination and permutation 
                problems about clothing, review similar problems about candy bars under 
                the 'More Practice' tab. Each candy bar question contains a detailed answer."),
              p(tags$b("Note: "), 
                "Warm weather clothing includes t-shirts and shorts. 
               Cold weather clothing includes hoodies and pants.
               Shoes can be worn in any type of weather."),
              br(),
              fluidRow(
                box(
                  title = strong("Permutation with Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("How many outfits could you make with the given wardrobe?"),
                  br(),
                  uiOutput("clothesQuestion1ans")
                ),
                box(
                  title = strong("Permutation without Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("How many outfits could you make with the given wardrobe, 
                  without rewearing any outfits?"),
                  br(),
                  uiOutput("clothesQuestion2ans")
                )
              ),
              fluidRow(
                box(
                  title = strong("Combination with Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("How many different outfits could you make with the given wardrobe?"),
                  br(),
                  uiOutput("clothesQuestion3ans")
                ),
                box(
                  title = strong("Combination without Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("How many different outfits could you make with the given wardrobe, 
              without rewearing any outfits?"),
              br(),
              uiOutput("clothesQuestion4ans")
                )
              ),
              fluidRow(
                box(
                  title = strong("Conditional combination with Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Given that there is warm weather, 
                  how many different outfits could you make with the given wardrobe?"),
                  br(),
                  uiOutput("clothesQuestion5ans")
                ),
                box(
                  title = strong("Conditional combination without Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Given that there is warm weather, 
                  how many different outfits could you make with the given wardrobe, 
                  without rewearing any outfits?"),
                  br(),
                  uiOutput("clothesQuestion6ans")
                )
              )
            ),
            tabPanel(
              withMathJax(),
              title = "Multiple Choice", 
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
                column(width = 12, 
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
                    inputId = "submit1",
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  ), 
                  
                ),
                column(
                  width = 2, 
                  uiOutput("mark")
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
                )
              ),
              fluidRow(
                column(
                  width = 12, 
                  br(), 
                  uiOutput("feedback")
                )
                
              ), 
              uiOutput("math3"),
              uiOutput("math4")
            )
          ),
          
          br(), 
          br(),
          br(), 
          
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goExp",
              label = "Game!",
              size = "large",
              icon = icon("gamepad"),
              style = "default"
            )
          ) 
          
        ),
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Poker Combinatorics"),
          br(),
          fluidRow(
            column(
              width = 2,
              align = "center",
              offset = 1,
              uiOutput("card1")
            ),
            column(
              width = 2,
              align = "center",
              offset = 0,
              uiOutput("card2")
            ),
            column(
              width = 2,
              align = "center",
              offset = 0,
              uiOutput("card3")
            ),
            column(
              width = 2,
              align = "center",
              offset = 0,
              uiOutput("card4")
            ),
            column(
              width = 2,
              align = "center",
              offset = 0,
              uiOutput("card5")
            )
          ),
          br(),
          textOutput("handPicker"),
          fluidRow(
            column(
              width = 6,
              # offset = 1,
              radioButtons(
                inputId = "pokerAnswers",
                label = "Click the 'New Hand' button to begin the poker questions.",
                choices =  character(0),
                selected = character(0)
              ), 
              fluidRow(
                column(
                  width = 2,
                  offset = 0,
                  bsButton(
                    inputId = "newHand",
                    label = "New Hand",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 2,
                  offset = 1, 
                  bsButton(
                    inputId = "submit",
                    label = "Submit Answer",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  ),
                ),
                column(
                  width = 2,
                  offset = 2, 
                  uiOutput("scoreImg")
                )
              ),
              
            ),
            column(
              width = 6,
              offset = 0,
              div(
                style = "text-align: center",
                textOutput("showScore"),
                br(),
                bsButton(
                  inputId = "showExpln",
                  label = "Answer Explanation", 
                  disabled = TRUE
                ), 
              ), 
              br(), 
              uiOutput("showExplnDisplay")
            )
          ),
          uiOutput("math1"),
          uiOutput("math2")
        ),
        
        ####  References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
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
            "Murray, W. (2021). Probability Choices: Combinations & Permutations.",
            tags$em("Educator"), 
            "from https://www.educator.com/mathematics/probability/murray/choices_-combinations-+-permutations.php"
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
  
  ## Set up navigation buttons ----
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Apply counting techniques to combinatorics and probability problems."
      )
    }
  )
  
  observeEvent(
    eventExpr = input$overviewToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites")
    }
  )
  
  observeEvent(
    eventExpr = input$prereqToExplore,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore")
    }
  )
  
  observeEvent(
    eventExpr = input$goExp,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game")
    }
  )
  
  ## Random Number Generator for Wardrobe ----
  observeEvent(
    eventExpr = input$newWardrobe,
    handlerExpr = {
      # Why not use randCounts <- sample(x = 2:7, size = 10, replace = TRUE)
      randomNumber1 <- sample(2:7, 1)
      randomNumber2 <- sample(2:7, 1)
      randomNumber3 <- sample(2:7, 1)
      randomNumber4 <- sample(2:7, 1)
      randomNumber5 <- sample(2:7, 1)
      randomNumber6 <- sample(2:7, 1)
      randomNumber7 <- sample(2:7, 1)
      randomNumber8 <- sample(2:7, 1)
      randomNumber9 <- sample(2:7, 1)
      randomNumber10 <- sample(2:7, 1)
      
      # output$clothing <- renderText(randCount[#])
      output$tshirt_solid_num <- renderText(randomNumber1)
      output$tshirt_stripe_num <- renderText(randomNumber2)
      output$hoodie_solid_num <- renderText(randomNumber3)
      output$hoodie_stripe_num <- renderText(randomNumber4)
      output$pants_solid_num <- renderText(randomNumber5)
      output$pants_stripe_num <- renderText(randomNumber6)
      output$shorts_solid_num <- renderText(randomNumber7)
      output$shorts_stripe_num <- renderText(randomNumber8)
      output$checker_shoes_num <- renderText(randomNumber9)
      output$psu_adidas_num <- renderText(randomNumber10)
      
      # What's happening here? Is there an easier way to do what you're tying to do?
      output$clothesQuestion1ans <- renderUI({
        withMathJax(
          sprintf(
            fmt = "\\(n^{r} = %d * %d * %d * %d * %d * %d * %d * %d * %d * %d = %d\\)",
            randomNumber1,
            randomNumber2,
            randomNumber3,
            randomNumber4,
            randomNumber5,
            randomNumber6,
            randomNumber7,
            randomNumber8,
            randomNumber9,
            randomNumber10,
            (randomNumber1 * randomNumber2 * randomNumber3 * randomNumber4 * randomNumber5 *randomNumber6 * randomNumber7 * randomNumber8 * randomNumber9 * randomNumber10)
          )
        )
      })
      output$clothesQuestion2ans <- renderUI({
        withMathJax(
          sprintf(
            fmt = "\\(_{n}P_{r} = %d * %d * %d * %d * %d * %d * %d * %d * %d * %d = %d\\)",
            (randomNumber1 - 1),
            (randomNumber2 - 1),
            (randomNumber3 - 1),
            (randomNumber4 - 1),
            (randomNumber5 - 1),
            (randomNumber6 - 1),
            (randomNumber7 - 1),
            (randomNumber8 - 1),
            (randomNumber9 - 1),
            (randomNumber10 - 1),
            ((randomNumber1 - 1) * (randomNumber2 - 1) * (randomNumber3 - 1) * (randomNumber4 - 1) * (randomNumber5 - 1) *(randomNumber6 - 1) * (randomNumber7 - 1) * (randomNumber8 - 1) * (randomNumber9 - 1) * (randomNumber10 - 1))
          )
        )
      })
      output$clothesQuestion3ans <- renderUI({
        withMathJax()
      })
      output$clothesQuestion4ans <- renderUI({
        withMathJax()
        (randomNumber1+randomNumber4)
      })
      output$clothesQuestion5ans <- renderUI({
        withMathJax()
        (randomNumber1+randomNumber5)
      })
      output$clothesQuestion6ans <- renderUI({
        withMathJax()
        (randomNumber1+randomNumber6)
      })
    }
  )
  
  ## Poker Page ----
  scoreCount <- reactiveVal(0)
  
  output$showScore <- renderText({
    paste("Your score is", scoreCount(), ".")
  })
  
  handNum <- reactiveVal(0)
  
  observeEvent(
    eventExpr = input$newHand,
    handlerExpr = {
      handNum(sample(x = 1:nrow(pokerHands), size = 1))
      updateRadioButtons(
        session = session,
        inputId = "pokerAnswers",
        label = pokerHands$question[handNum()],
        choices = c(pokerHands$mathcodeCorrect[handNum()],
                    pokerHands$mathcodeAlt1[handNum()],
                    pokerHands$mathcodeAlt2[handNum()],
                    pokerHands$mathcodeAlt3[handNum()]),
        selected = character(0)
      )
      output$math1 <- renderUI({withMathJax()})
      output$math2 <- renderUI({withMathJax()})
      output$showExplnDisplay <- renderUI({
        return = NULL
      })
      updateButton(
        session = session, 
        inputId = "showExpln", 
        disabled = TRUE)
      
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  

  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      if (!is.null(input$pokerAnswers)) {
        correct <- input$pokerAnswers == pokerHands$mathcodeCorrect[handNum()]
        if (correct) {
          scoreCount(scoreCount() + 3)
          output$scoreImg <- renderIcon(icon = "correct", width = 50)
      } else {
        scoreCount(scoreCount() - 1)
        output$scoreImg <- renderIcon(icon = "incorrect", width = 50)}}
      updateButton(
        session = session, 
        inputId = "showExpln", 
        disabled = FALSE)
        })
    
  
  observeEvent(
    eventExpr = input$showExpln,
    handlerExpr = {
      output$math1 <- renderUI({withMathJax()})
      output$math2 <- renderUI({withMathJax()})
      output$showExplnDisplay <- renderUI({
        p(tags$b("Answer Explanation: "), withMathJax(pokerHands$ansExpln[handNum()]))
      })
    })
  # observeEvent(
  #   eventExpr = input$showExpln,
  #   handlerExpr = {
  #     sendSweetAlert(
  #       session = session,
  #       title = "Answer Explanation",
  #       text = pokerHands$ansExpln[handNum()],
  #       closeOnClickOutside = TRUE,
  #       showCloseButton = TRUE
  #     )
  #     output$math1 <- renderUI({withMathJax()})
  #     output$math2 <- renderUI({withMathJax()})
  #   })
  
  output$card1 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(src = pokerHands$card1[handNum()],
          width = "100%",
          contentType = "image/png",
          alt = pokerHands$text1[handNum()])
    }
  })
  
  output$card2 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(src = pokerHands$card2[handNum()],
          width = "100%",
          contentType = "image/png",
          alt = pokerHands$text2[handNum()])
    }
  })
  
  output$card3 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(src = pokerHands$card3[handNum()],
          width = "100%",
          contentType = "image/png",
          alt = pokerHands$text3[handNum()])
    }
  })
  
  output$card4 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(src = pokerHands$card4[handNum()],
          width = "100%",
          contentType = "image/png",
          alt = pokerHands$text4[handNum()])
    }
  })
  
  output$card5 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(src = pokerHands$card5[handNum()],
          width = "100%",
          contentType = "image/png",
          alt = pokerHands$text5[handNum()])
    }
  })
  
  ### Explore Page Practice ----
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
  
  Qs <<- nrow(questionBank)
  Qs_array <<- c(1:Qs)
  id <- 1
  
  # Reset button
  observeEvent(input$restart, {
    withMathJax()
    updateButton(
      session = session, 
      inputId = "submit1", 
      disabled = FALSE)
    updateButton(
      session = session, 
      inputId = "nextq", 
      disabled = FALSE)
    updateButton(
      session = session, 
      inputId = "restart", 
      disabled = FALSE)
    
    
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
      status = "game"
    )
    output$math3 <- renderUI({
      withMathJax()
    })
    output$math4 <- renderUI({
      withMathJax()
    })
    output$mark <- renderUI({
      img(src = NULL, width = 50)
    })
    
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
    output$math3 <- renderUI({
      withMathJax()
    })
    output$math4 <- renderUI({
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
        updateButton(session, "submit1", disabled = FALSE)
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
        output$math3 <- renderUI({
          withMathJax()
        })
        output$math4 <- renderUI({
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
          inputId = "submit1", 
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
          status = "game"
        )
        output$math3 <- renderUI({
          withMathJax()
        })
        output$math4 <- renderUI({
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
        inputId = "submit1", 
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
      output$math3 <- renderUI({
        withMathJax()
      })
      output$math4 <- renderUI({
        withMathJax()
      })
    }
  })
  
  ### SUBMIT BUTTON###
  observeEvent(input$submit1, {
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
          inputId = "submit1", 
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
        inputId = "submit1", 
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
  
  ### PRINT HINTS###
  observeEvent(
    eventExpr = input$hint, 
    handlerExpr = {
      output$math3 <- renderUI({
        withMathJax()
      })
      output$math4 <- renderUI({
        withMathJax()
      })
      withMathJax()
      output$hintDisplay <- renderUI({
        p(tags$b("Hint:"), questionBank[id, "Hint"])
      })
    })
}
# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
