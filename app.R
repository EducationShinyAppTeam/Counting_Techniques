# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

#Load explore and poker question banks
questionBank <- read.csv("exploreQuestions.csv", stringsAsFactors = FALSE)

pokerHands <- read.csv("pokerquestionbank.csv") 
cardBacks <- function(){
  return(
    img(
      src = "pokercard-back.png",
      width = "100%",
      contentType = "image/png", 
      alt = "A card dealt face down" 
    )
  )
}

## These functions are the bullet points for the worked examples in the  
## prerequsites page. They give descriptions for the different types of counting
## techniques. 
permutation <- function(){
  return(tags$li(
    "Since the candy bars are different, order matters. Therefore, we use a
    permutation."
  ))
}
combination <- function(){
  return(tags$li(
    "Since the candy bars are identical, order does not matter. Therefore, we
    use a combination."
  ))
}
replacement <- function(){
  return(tags$li(
    "Since students can receive more than 1 candy bar, there is replacement."
  ))
}
noReplace <- function(){
  return(tags$li(
    "Since students cannot receive more than 1 candy bar, there is no
    replacement."
  ))
}

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Counting Techniques",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Counting_Techniques") 
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
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
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
            counting principles."),
          h2("Instructions"),
          tags$ol(
            tags$li("Use the Prerequistes page to review the different counting 
                    principles and the keywords to be aware of when solving
                    problems."),
            tags$li("Use the Explore page to see the difference between the
                    counting techniques mentioned. You can further test your
                    knowledge in the Multiple Choice page."),
            tags$li("Use the Challenge page to apply counting techniques in 
                    poker-related problems."), 
            tags$li("For poker, score three points for each question you get
                    right and lose a point when you get one wrong. Win by
                    getting 20 points!")
          ),
          br(), 
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
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Kellien Peritz
            and Shravani Samala.",
            br(),
            "We would like to extend a special thanks to Dr. Dennis Pearl for
            providing the question bank.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 10/21/2021 by SJS")
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
                    way you solve a problem.")
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
              p("Number of ways to pick \\(r\\) things from \\(n\\)
                possibilities:"),
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
              p("Number of ways to pick \\(r\\) things from \\(n\\)
                possibilities:"),
              tags$ul(
                tags$li("\\({}_{n}P_{r}=\\dfrac{n!}{(n-r)!}\\)"),
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
              p("Number of ways to pick \\(r\\) things from \\(n\\)
                possibilities:"),
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
              p("Number of ways to pick \\(r\\) things from \\(n\\) 
                possibilities:"),
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
              p("Number of distinguishable permutations of \\(n\\) objects:"),
              tags$ul( 
                tags$li("\\(n_{1}\\) of the \\(1^{st}\\) object."),
                tags$li("\\(n_{2}\\) of the \\(2^{nd}\\) object."),
                tags$li("\\(n_{k}\\) of the \\(k^{th}\\) object."),
                tags$li("Where \\(\\sum_{i=1}^{k=n_i} = n\\) is 
                      \\(\\binom{n}{{n_{1}}{n_{2}}\\cdots{n_{k}}}=\\dfrac{n!}
                        {{n_{1}!}{n_{2}!}{n_{3}!}\\cdots{n_{k}!}}\\)"), 
                tags$li("Example: How many ordered arrangements are there of the
                        letters in MISSISSIPPI?", 
                  tags$ul(
                    tags$li("There is one letter M, 4 letter I's, 4 letter S's
                            and 2 letter P's. The letters such as I, S, and P, 
                            we cannot distinguish between. Therefore, the
                            number of ordered arrangements for the word 
                            MISSISSIPPI is \\(\\dfrac{11!}{1!4!4!2!}\\)"
                    ) 
                  )
                )
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
                        ("\\({E_{1},E_{2}...E_{m}=}\\)")), 
                tags$li("\\({{n_{1}}\\times{n_{2}}\\times{n_{3}}\\times\\cdots
                        \\times{n_{m}}= n!}\\)"), 
                tags$li("Example: if each license plate needs three letters 
                        and four numbers, how many possible license plates
                        can be stamped? (\\(ABC 1234\\) is one example)",
                  tags$ul(
                    tags$li("The first three spots can each be filled by 
                            three letters. The last four spots can be filled
                            by the numbers 0-9, which gives us 10 possible
                            numbers. Therefore, the possible number of 
                            lisence plates are: \\({26}\\times{26}\\times{26}
                            \\times{10}\\times{10}\\times{10}\\times{10}\\)"
                    )
                  )
                )       
              )
            )
          ),
          fluidRow(
            box(
              title = strong("Using Set Theory"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              p("Set theory operations are valuable for counting the number of
                elements in a set. As examples, if \\(|A|\\) represents the
                number of elements in the set A then"),
              tags$ul( 
                tags$li("\\(|A|\\cup{|B|} = |A| + |B| - |A\\cap{B}|\\)"),
                tags$li("\\(|A| = |AB_1| + |AB_2| + ...+ |AB_k|\\) if 
                        \\(B_1,...,B_k\\) are mutually exclusive and
                        exhaustive."),
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
          p("Use worked example tab to see how a problem's wording corresponds 
            to using the counting techniques equations. Then, test your
            knowledge in the multiple choice tab!"), 
          tabsetPanel(
            id = "exploreTabs", 
            type = "tabs", 
  
            ##### Candy Tab ----
            tabPanel(
              title = "Worked Examples",
              br(),
              ###### Candy bar PNGs + bttn ---- 
              fluidRow(
                column(
                  width = 3,
                  align = "center",
                  offset = 0,
                  tags$img(
                    src = "greenBar.png",
                    alt = "Mint chocolate bar",
                    width = "60%", 
                    p("Mint chocolate bar")
                  )
                ),
                column(
                  width = 3,
                  align = "center",
                  offset = 0,
                  tags$img(
                    src = "blueBar.png",
                    alt = "Blueberry chocolate bar",
                    width = "60%", 
                    p("Blueberry chocolate bar")
                  )
                ),               
                column(
                  width = 3,
                  align = "center",
                  offset = 0,
                  tags$img(
                    src = "pinkBar.png",
                    alt = "Strawberry chocolate bar",
                    width = "60%", 
                    p("Strawberry chocolate bar")
                  )
                ),
                column(
                  width = 3,
                  align = "center",
                  offset = 0,
                  tags$img(
                    src = "whiteBar.png",
                    alt = "White chocolate bar",
                    width = "60%", 
                    p("White chocolate bar")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 3,
                  offset = 0,
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "newClass",
                      label = "New class",
                      size = "large",
                      style = "default"
                    )
                  )
                ),  
                column(
                  width = 9,
                  offset = 0,
                  uiOutput("prompt")
                )
              ),
              br(),
              ###### Candy bar Qs ----
              fluidRow(
                box(
                  title = strong("Permutation with Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Distribute 4 different flavored candy bars to the class. 
                    You are willing to give some students more than 1 candy bar. 
                    How many ways can you distribute the candy bars?"),
                  p("Press button to reveal answer!"),
                  tags$ul( 
                    div(
                      style = "text-align: center;",
                      bsButton(
                        inputId = "showAnswerA1", 
                        label = "Show Answer!", 
                        size = "large", 
                        disabled = TRUE, 
                        style = "default"
                      )
                    ), 
                    br(), 
                    uiOutput("candyA1"),
                    permutation(),
                    replacement()
                  )
                ),
                box(
                  title = strong("Permutation without Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Distribute 4 different flavored candy bars to the class.
                    You do not want to give any student more than 1 candy bar. 
                    How many ways can you distribute the candy bars?"),
                  p("Press button to reveal answer!"),
                  tags$ul(
                    div(
                      style = "text-align: center;",
                      bsButton(
                        inputId = "showAnswerA2", 
                        label = "Show Answer!", 
                        size = "large", 
                        disabled = TRUE, 
                        style = "default"
                      )
                    ), 
                    br(), 
                    uiOutput("candyA2"),
                    permutation(),
                    noReplace()
                  ), 
                )
              ),
              fluidRow(
                box(
                  title = strong("Combination with Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Distribute 4 identical candy bars to the class. 
                    You are willing to give some students more than 1 candy bar. 
                    How many ways can you distribute the candy bars?"),
                  p("Press button to reveal answer!"),
                  tags$ul(
                    div(
                      style = "text-align: center;",
                      bsButton(
                        inputId = "showAnswerA3", 
                        label = "Show Answer!", 
                        size = "large", 
                        disabled = TRUE, 
                        style = "default"
                      )
                    ), 
                    br(), 
                    uiOutput("candyA3"),
                    combination(),
                    replacement()
                  )
                ),
                box(
                  title = strong("Combination without Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Distribute 4 identical candy bars to the class. 
                    You do not want to give any student more than 1 candy bar. 
                    How many ways can you distribute the candy bars?"),
                  p("Press button to reveal answer!"),
                  tags$ul(
                    div(
                      style = "text-align: center;",
                      bsButton(
                        inputId = "showAnswerA4", 
                        label = "Show Answer!", 
                        size = "large", 
                        disabled = TRUE, 
                        style = "default"
                      )
                    ), 
                    br(), 
                    uiOutput("candyA4"),
                    combination(),
                    noReplace()
                  )
                )
              ) , 
              fluidRow(
                box(
                  title = strong("Distinguishable Permutations"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("There are 20 candy bars consisting of all four flavors 
                    available to pass out to the students. If each flavor is to 
                    be rewarded to five students, how many ways can the candy bars
                    be rewarded the students?"),
                  p("Press button to reveal answer!"),
                  tags$ul(
                    div(
                      style = "text-align: center;",
                      bsButton(
                        inputId = "showAnswerA5", 
                        label = "Show Answer!", 
                        size = "large", 
                        disabled = TRUE, 
                        style = "default"
                      )
                    ), 
                    br(), 
                    uiOutput("candyA5"), 
                  )
                ),
                box(
                  title = strong("Multiplication Rule"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("You have 3 mint 4 blueberry, 4 strawberry, and 1 lemon candy
                    bar. How many ways can you distribute 4 different flavored
                    candy bars to one
                    student?"),
                  p("Press button to reveal answer!"),
                  tags$ul(
                    div(
                      style = "text-align: center;",
                      bsButton(
                        inputId = "showAnswerA6", 
                        label = "Show Answer!", 
                        size = "large", 
                        disabled = TRUE, 
                        style = "default"
                      )
                    ), 
                    br(), 
                    uiOutput("candyA6"),
                    
                  )
                )
              )
            ),
            ##### MCQ tab ----
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
                  br()
                )
              ), 
              fluidRow(
                column(
                  width = 12, 
                  radioGroupButtons(
                    inputId = "mc1",
                    label = "Which expression addresses the question?",
                    status = "game",
                    direction = "vertical",
                    selected = character(0),
                    checkIcon = list(
                      yes = icon("check-square"),
                      no = icon("square")
                    ),
                    choices = list(
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
                  )
                ), 
                column(
                  width = 2, 
                  bsButton(
                    inputId = "submit1",
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
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
                  )
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
        #### Poker Page ---- 
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Poker Combinatorics"),
          p("Use the counting techniques you learned and reviewed in the explore
            page to answer the following poker-style questions. Answering a
            question correctly earns you three points while answering a question
            incorrectly loses you 1 point. Accumulate 20 points to win the game. 
            However, if you score -10 points, you lose the game and can replay!"), 
          
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
              ialign = "center",
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
          fluidRow(
            column(
              width = 12,
              align = "center",
              offset = 0,
              ## Fix: the caption text isn't getting displayed
              textOutput("caption"),
            )
          ),
          br(),
          br(), 

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
              ), 
            ), 
            column(
              width = 6
            ), 
            column(
              width = 4,
              #offset = 8,
              div(
                style = "text-align: center",
                textOutput("showScore"),
              ), 
            ),
          ), 
          br(), 
          fluidRow(
            column(
              width = 8, 
              radioButtons(
                inputId = "pokerAnswers",
                label = "Click the 'New Hand' button to begin the poker questions.",
                choices =  character(0),
                selected = character(0)
              ), 
            ), 
            column(
              width = 4, 
              uiOutput("showExplnDisplay")
            )
          ), 
          br(), 
          fluidRow(
            column(
              width = 2,
              offset = 0,
              bsButton(
                inputId = "submit",
                label = "Submit Answer",
                size = "large",
                style = "default",
                disabled = TRUE
              )
            ),
            column(
              width = 2,
              offset = 0,
              uiOutput("scoreImg")
            ), 
            column(
              width = 5
            ), 
            column(
              width = 3,
              bsButton(
                inputId = "showExpln",
                label = "Answer Explanation",
                size = "large",
                disabled = TRUE
              )
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
  boastUtils::typesetMath(session = session)
  
  ## Set up navigation bttns ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Apply counting techniques to solve problems."
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
    })
  
  ## Candy Page -----
  classNum <- reactiveVal(0)
  
  output$prompt <- renderUI({
    "Click the new class button to begin"
  })
  
  observeEvent(
    eventExpr = input$newClass,
    handlerExpr = {
      ##Choose a number between 18 and 36 to be the class size for the candy bar
      ##part of the explore page 
      classNum(sample(18:36, 1))
      
      output$prompt <- renderUI({
        withMathJax(paste(
          "You are the teacher of a class of",
          classNum(),
          "students. You reward your students with candy bars of possibly different
          flavors. Use the examples below to see the different ways of giving out
          the candy bars."
        ))
      })
      
      updateButton(
        session = session, 
        inputId = "showAnswerA1", 
        disabled = FALSE
      )
      updateButton(
        session = session, 
        inputId = "showAnswerA2", 
        disabled = FALSE
      )
      updateButton(
        session = session, 
        inputId = "showAnswerA3", 
        disabled = FALSE
      )
      updateButton(
        session = session, 
        inputId = "showAnswerA4", 
        disabled = FALSE
      )
      updateButton(
        session = session, 
        inputId = "showAnswerA5", 
        disabled = FALSE
      )
      updateButton(
        session = session, 
        inputId = "showAnswerA6", 
        disabled = FALSE
      )
      output$candyA1 <- renderUI({NULL})
      output$candyA2 <- renderUI({NULL})
      output$candyA3 <- renderUI({NULL})
      output$candyA4 <- renderUI({NULL})
  })
  
  #### Set up buttons for candy page ----
  observeEvent(
    eventExpr = input$showAnswerA1, 
    handlerExpr = {
      output$candyA1 <- renderUI({
        withMathJax(tags$li(paste(sprintf(
          fmt = "\\(n^{r} = %d ^4\\)",
          classNum()
        ))))
      })
    }
  )
  
  observeEvent(
    eventExpr = input$showAnswerA2, 
    handlerExpr = {
      output$candyA2 <- renderUI({
        withMathJax(tags$li(paste(sprintf(
          fmt = "\\(_{n}P_{r} = \\dfrac{n!}{(n-r)!} = \\dfrac{(%d)!}{(%d)!}\\)",
          classNum(),
          (classNum() - 4)
        ))))
      })
    }
  )
  
  observeEvent(
    eventExpr = input$showAnswerA3, 
    handlerExpr = {
      output$candyA3 <- renderUI({
        withMathJax(tags$li(paste(sprintf(
          fmt = "\\(\\binom{n+r-1}{r} = \\dfrac{(n+r-1)!}{r!(n-1)!} = \\dfrac{(%d)!}{4!(%d)!}\\)",
          (classNum() + 4 - 1),
          (classNum() - 1)
        ))))
      })
    }
  )
  
  observeEvent(
    eventExpr = input$showAnswerA4, 
    handlerExpr = {
      output$candyA4 <- renderUI({
        withMathJax(tags$li(paste(sprintf(
          fmt = "\\(\\binom{n}{r} = \\dfrac{n!}{r!(n-r)!} = \\dfrac{(%d)!}{4!(%d)!}\\)",
          classNum(),
          (classNum() - 4)
        ))))
      })
    }
  )
  
  observeEvent(
    eventExpr = input$showAnswerA5, 
    handlerExpr = {
      output$candyA5 <- renderUI({
        withMathJax(tags$li(paste(sprintf(
          fmt = "\\(\\dfrac{20!}{5!5!5!5!}\\)",
          classNum(),
          (classNum() - 4)
        ))))
      })
    }
  )
  
  observeEvent(
    eventExpr = input$showAnswerA6, 
    handlerExpr = {
      output$candyA6 <- renderUI({
        withMathJax(tags$li(paste(sprintf(
          fmt = "\\(3\\cdot{4}\\cdot{4}\\cdot{1}\\)",
          classNum(),
          (classNum() - 4)
        ))))
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
      
      ## Fix: As mentioned above, this isn't getting displayed.
      output$caption <- renderText(pokerHands$caption[handNum()])
      
      ansChoices <- c(pokerHands$mathcodeCorrect[handNum()],
                      pokerHands$mathcodeAlt1[handNum()],
                      pokerHands$mathcodeAlt2[handNum()],
                      pokerHands$mathcodeAlt3[handNum()])
      
      randomAnsChoices <- sample(ansChoices, 4)
      
      updateRadioButtons(
        session = session,
        inputId = "pokerAnswers",
        label = pokerHands$question[handNum()],
        choices = c(randomAnsChoices),
        selected = character(0)
      )
      output$math1 <- renderUI({withMathJax()})
      output$math2 <- renderUI({withMathJax()})
      output$showExplnDisplay <- renderUI({NULL})
      
      ## Fix: Use renderIcon() 
      output$scoreImg <- renderIcon()
      output$scoreImg <- renderUI({
        img(src = NULL, width = 50)
      })
      updateButton(
        session = session, 
        inputId = "showExpln", 
        disabled = TRUE)
      updateButton(
        session = session, 
        inputId = "submit", 
        disabled = FALSE)
      updateButton(
        session = session, 
        inputId = "showAnswerA2", 
        disabled = FALSE)
      
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
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      
      ### Game Over Checks
      if (scoreCount() >= 20) {
        sendSweetAlert(
          session = session,
          title = "You Win!",
          type = "success",
          text = "You have won the game! Congrats!"
        )
        scoreCount(0)
        updateButton(
          session = session,
          inputId = "submit",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "newHand",
          disabled = FALSE
        )
        
      } else if (scoreCount()  <= -10) {
        sendSweetAlert(
          session = session,
          title = "You lost.",
          type = "error",
          text = "You have lost the game. Please try again.",
          closeOnClickOutside = FALSE
        )
        scoreCount(0)
        updateButton(
          session = session,
          inputId = "newHand",
          disabled = FALSE
        )
        updateButton(
          session = session,
          inputId = "submit",
          disabled = TRUE
        )
      }
      
    })
  
  observeEvent(
    eventExpr = input$showExpln,
    handlerExpr = {
      output$math1 <- renderUI({withMathJax()})
      output$math2 <- renderUI({withMathJax()})
      output$showExplnDisplay <- renderUI({
        p(tags$b("Answer Explanation: "), withMathJax(pokerHands$ansExpln[handNum()]))
      })
      
      updateButton(
        session = session,
        inputId = "sumbit",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "newHand",
        disabled = FALSE
      )
      
    })
  
  
  output$card1 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(
        src = pokerHands$card1[handNum()],
        width = "100%",
        contentType = "image/png",
        alt = pokerHands$text1[handNum()]
      )
    }
  })
  
  output$card2 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(
        src = pokerHands$card2[handNum()],
        width = "100%",
        contentType = "image/png",
        alt = pokerHands$text2[handNum()]
      )
    }
  })
  
  output$card3 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(
        src = pokerHands$card3[handNum()],
        width = "100%",
        contentType = "image/png",
        alt = pokerHands$text3[handNum()]
      )
    }
  })
  
  output$card4 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(
        src = pokerHands$card4[handNum()],
        width = "100%",
        contentType = "image/png",
        alt = pokerHands$text4[handNum()]
      )
    }
  })
  
  output$card5 <- renderUI({
    if (handNum() == 0) {
      cardBacks()
    } else {
      img(
        src = pokerHands$card5[handNum()],
        width = "100%",
        contentType = "image/png",
        alt = pokerHands$text5[handNum()]
      )
    }
  })
  
  ## Explore Page Practice ----
  qsArray <- c(1:nrow(questionBank))
  
  qs <- nrow(questionBank)
  qsArray <- c(1:qs)
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
      hint <- withMathJax(questionBank[id, "Hint"])
      return(paste(questionBank[id, "Scenario"], questionBank[id, "Question"]))
    })
    
    output$hint <- renderUI({
      withMathJax()
      hint <<- withMathJax(questionBank[id, "Hint"])
      return(questionBank[id, "Hint"])
    })
    
    updateRadioGroupButtons(
      session = session, 
      inputId = "mc1",
      choices = list(
        questionBank[id, "A"],
        questionBank[id, "B"],
        questionBank[id, "C"],
        questionBank[id, "D"] 
      ),
      selected = character(0),
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square")
      ),
      status = "game"
    )
    output$math3 <- renderUI({
      withMathJax()
    })
    output$math4 <- renderUI({
      withMathJax()
    })
    ## Fix: use renderIcon()
    output$mark <- renderUI({
      img(src = NULL, width = 50)
    })
    
  })
  
  # Print out a question
  output$question <- renderUI({
    withMathJax()
    id <- sample(qsArray, 1, replace = FALSE, prob = NULL)
    qsArray <- qsArray[!qsArray %in% id]
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
        no = icon("square")
      ),
      status = "game"
    )
    output$math3 <- renderUI({
      withMathJax()
    })
    output$math4 <- renderUI({
      withMathJax()
    })
    hint <- withMathJax(questionBank[id, "Hint"])
    return(withMathJax(paste(questionBank[id, "Scenario"], questionBank[id, "Question"])))
  })
  
  ### NEXT QUESTION BUTTON###
  observeEvent(input$nextq, {
    withMathJax()
    if (length(qsArray) > 1) {
      id <- sample(qsArray, 1, replace = FALSE, prob = NULL)
      qsArray <- qsArray[!qsArray %in% id]
      hint <- questionBank["Hint"]
        updateButton(
          session = session, 
          inputId = "submit1", 
          disabled = FALSE)
        output$question <- renderUI({
          return(paste(questionBank[id, "Scenario"], questionBank[id, "Question"]))
        })
        
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
            no = icon("square")
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

      ##HINT###
      output$hintDisplay <- renderUI({NULL})
      output$feedback <- renderUI({NULL})
    }
    else if (length(qsArray) == 1) {
      id <- qsArray[1]
      qsArray <- qsArray[!qsArray %in% id]
      hint <- questionBank[id, "Hint"]
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
            no = icon("square")
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

      ##HINT###
      output$hintDisplay <- renderUI({NULL})
      output$feedback <- renderUI({NULL})
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
      output$question <- renderUI({NULL})
      output$hintDisplay <- renderUI({NULL})
      output$feedback <- renderUI({NULL})
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
          no = icon("square")
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
  observeEvent(
    eventExpr = input$submit1, 
    handlerExpr = {
    withMathJax()
    letterAnswer <- questionBank[id, "Answer"]
    cAnswer <- questionBank[id, letterAnswer]
    mc1Length <- length(input$mc1)
    
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
