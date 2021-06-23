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
<<<<<<< HEAD
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
=======
          tabsetPanel(
            
            ##### COUNTING TAB ----
            tabPanel(
              title = "Counting Techniques",
              br(),
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
                    tags$li("\\(_{n+r-1}C_{r}=\\dfrac{(n+r-1)!}{r!(n-1)!}\\)"),
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
                    tags$li("\\(_{n}C_{r}=\\dfrac{n!}{r!(n-r)!}\\)"),
                    tags$li("Unordered subsets without replacement")
                  )
                )
>>>>>>> d290036049001bf444b8b0689659fe7da191f892
              )
            ),
            
            ##### PROBABILITY TAB ----
            tabPanel(
              title = "Binomial Probability",
              br(),
              h3("Applying counting techniques to binomial probability"),
              br(),
              fluidRow(
                box(
                  title = strong("Exactly"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 4,
                  p("The probability of obtaining exactly", tags$em("r"), "events 
                in ", tags$em("n"), "trials = "), 
                p("\\(P(X=r)=\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\)"), 
                p("where ", tags$em("p"), "is the probability of success"), 
                br(), 
                br()
                ),
                
                box(
                  title = strong("At Most"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 4,
                  p("The probability of obtaining ", tags$strong("at most"), tags$em("r"), 
                    "from", tags$em("n"), "trials = "), 
                  p("\\(P(X\\le{r})=\\sum_{i=0}^r\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\)"), 
                  p("where ", tags$em("p"), "is the probability of success"), 
                  br(), 
                  br()
                ),
                
                box(
                  title = strong("At Least"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 4,
                  p("The probability of obtaining ", tags$strong("at least"), tags$em("r"), 
                    "from", tags$em("n"), "trials = "),
                  p("\\(P(X\\ge{r})=\\sum_{i=r}^n\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\) or"), 
                  p("\\(1-P(X\\le{r})\\)"),
                  p("where ", tags$em("p"), "is the probability of success")
                )
              )
            )
          ), 
          br(), 
          div(
            style = "text-align: center",
            bsButton(
              inputId = "PrereqToExplore",
              label = "Explore",
              size = "large",
              icon = icon("wpexplorer"),
              style = "default"
            )
          )
        ),
        
        #### Explore/Clothing Page ---- 
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the Concept"),
          tabsetPanel(
            
            ##### CLOTHING TAB ----
            tabPanel(
              title = "Practice Combinations & Permutations",
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
            
            ##### GENERAL PRACTICE TAB ----
            tabPanel(
              title = "More Practice", 
              br(),
              h3("Questions with Answer Explanations"), 
              fluidRow(
                box(
                  title = strong("Permutation with Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Distribute 5 different candy bars to 20 children. 
                  We are willing to give some children more than 1 candy bar. 
                  How many ways can we distribute the candy bars?"),
                  tags$ul( 
                    tags$li("Since the candy bars are different, order matters. 
                          Therefore, we use a permutation."),
                    tags$li("Since children can receive more than 1 candy bar, 
                          there is replacement."),
                    tags$li("\\(n^{r} = 20^{5}\\)")
                  )
                ),
                box(
                  title = strong("Permutation without Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Distribute 5 different candy bars to 20 children. 
                  We do not want to give any child more than 1 candy bar. 
                  How many ways can we distribute the candy bars?"),
                  tags$ul( 
                    tags$li("Since the candy bars are different, order matters. 
                          Therefore, we use a permutation."),
                    tags$li("Since children cannot receive more than 1 candy bar, 
                          there is no replacement."),
                    tags$li("\\(_{n}P_{r} = _{20}P_{5} =\\dfrac{20!}{15!}\\)")
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
                  p("Distribute 5 identical candy bars to 20 children. 
                  We are willing to give some children more than 1 candy bar. 
                  How many ways can we distribute the candy bars?"),
                  tags$ul( 
                    tags$li("Since the candy bars are identical, order does not 
                          matter. Therefore, we use a combination."),
                    tags$li("Since children can receive more than 1 candy bar, 
                          there is replacement."),
                    tags$li("\\(_{n+r-1}C_{r} = _{24}C_{5} = \\dfrac{24!}{5!(19)!}\\)")
                  )
                ),
                box(
                  title = strong("Combination without Replacement"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 6,
                  p("Distribute 5 identical candy bars to 20 children. 
                  We do not want to give any child more than 1 candy bar. 
                  How many ways can we distribute the candy bars?"),
                  tags$ul( 
                    tags$li("Since the candy bars are identical, order does not 
                          matter. Therefore, we use a combination."),
                    tags$li("Since children cannot receive more than 1 candy bar, 
                          there is no replacement."),
                    tags$li("\\(_{n}C_{r} = _{20}C_{5} = \\dfrac{20!}{5!(15)!}\\)")
                  )
                )
              )
            )
          )
        ),
        
        #### Game/Poker Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Poker Probability"),
          fluidRow(
            column(
              width = 2,
              align = "center",
              offset = 1,
              imageOutput("card1")
            ),
            column(
              width = 2,
              align = "center",
              offset = 0,
              imageOutput("card2")
            ),
            column(
              width = 2,
              align = "center",
              offset = 0,
              imageOutput("card3")
            ),
            column(
              width = 2,
              align = "center",
              offset = 0,
              imageOutput("card4")
            ),
            column(
              width = 2,
              align = "center",
              offset = 0,
              imageOutput("card5")
            )
          ),
          br(),
          fluidRow(
            div(
              style = "text-align: center",
              bsButton(
                inputId = "newHand",
                label = "New Hand",
                size = "large",
                style = "default"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              offset = 0,
              wellPanel(
                print("tic-tac-toe will appear here")
              )
            ),
            column(
              width = 6,
              offset = 0,
              textOutput("prompt"),
              wellPanel(
                radioButtons(
                  inputId = "promptAnsOptions",
                  label = "Find the probability of being dealt the hand displayed.",
                  choices =  c("Choice A","Choice B", "Choice C")
                )
              )
            )
          )
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
  
  ## Set up navigation BUTTONS ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Apply counting principles to combinatorics and probability problems."
      )
    }
  )
  
  observeEvent(
    eventExpr = input$OverviewToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites")
    }
  )
  
  observeEvent(
    eventExpr = input$PrereqToExplore,
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
      })
      output$clothesQuestion4ans <- renderUI({
        (randomNumber1+randomNumber4)
      })
      output$clothesQuestion5ans <- renderUI({
        (randomNumber1+randomNumber5)
      })
      output$clothesQuestion6ans <- renderUI({
        (randomNumber1+randomNumber6)
      })
      
    }
  )
  
  ## Poker Cards ----
  
  observeEvent(
    eventExpr = input$newHand,
    handlerExpr = {
      randomNumber11 <- sample(1:9, 1)
      
      ### 1 Pair ----
      if (randomNumber11 == 1) {
        output$card1 <- renderImage({
          list(src = "a-club.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of clubs")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "a-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of hearts")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "9-spade.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "9 of spades")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "8-diamond.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "8 of diamonds")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "7-club.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "7 of clubs")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a 1 PAIR. 
                        Find the probability of this poker hand."),
          choices = c(
            "nicki minaj", 
            "megan stallion",
            "saweetie"
          )
        )
      }
      
      ### 2 Pairs ----
      if (randomNumber11 == 2) {
        output$card1 <- renderImage({
          list(src = "k-diamond.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "King of diamonds")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "k-club.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "King of clubs")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "q-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Queen of hearts")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "q-spade.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Queen of spades")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "j-diamond.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Jack of diamonds")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a 2 PAIRS. 
                        Find the probability of this poker hand."),
          choices = c(
            "dog",
            "cat",
            "bird"
          )
        )
      }
      
      ### 3 of a kind ----
      if (randomNumber11 == 3) {
        output$card1 <- renderImage({
          list(src = "a-club",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of clubs")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "a-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of hearts")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "a-spade.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of spades")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "2-diamond.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "2 of diamonds")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "7-club.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "7 of clubs")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a 3 OF A KIND. 
                        Find the probability of this poker hand."),
          choices = c(
            "happy",
            "sad",
            "tired"
          )
        )
      }
      
      ### Straight ----
      if (randomNumber11 == 4) {
        output$card1 <- renderImage({
          list(src = "5-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "5 of hearts")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "6-club.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "6 of clubs")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "7-diamond.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "7 of diamonds")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "8-spade.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "8 of spades")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "9-club.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "9 of clubs")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a STRAIGHT. 
                        Find the probability of this poker hand."),
          choices = c(
            "bachelors",
            "masters",
            "doctorate"
          )
        )
      }
      
      ### Flush ----
      if (randomNumber11 == 5) {
        output$card1 <- renderImage({
          list(src = "2-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "2 of hearts")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "4-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "4 of hearts")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "6-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "6 of hearts")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "8-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "8 of hearts")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "K-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "King of hearts")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a FLUSH. 
                        Find the probability of this poker hand."),
          choices = c(
            "morning",
            "afternoon",
            "evening"
          )
        )
      }
      
      ### Full House ----
      if (randomNumber11 == 6){
        output$card1 <- renderImage({
          list(src = "a-spade.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of spades")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "a-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of hearts")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "a-club.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of clubs")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "k-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "King of hearts")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "K-spade.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "King of spades")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a FULL HOUSE. 
                        Find the probability of this poker hand."),
          choices = c(
            "dababy",
            "21 savage",
            "tyga"
          )
        )
      }
      
      ### 4 of a kind ----
      if (randomNumber11 == 7){
        output$card1 <- renderImage({
          list(src = "a-spade.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of spades")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "a-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of hearts")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "a-club.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of clubs")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "a-diamond.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of diamonds")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "2-spade.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "2 of spades")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a 4 OF A KIND. 
                        Find the probability of this poker hand."),
          choices = c(
            "lil nasx",
            "lil uzi",
            "lil wayne"
          )
        )
      }
      
      ### Straight Flush ----
      if (randomNumber11 == 8) {
        output$card1 <- renderImage({
          list(src = "5-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "5 of hearts")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "6-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "6 of hearts")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "7-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "7 of hearts")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "8-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "8 of hearts")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "9-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "9 of hearts")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a STRAIGHT FLUSH. 
                        Find the probability of this poker hand."),
          choices = c(
            "kim",
            "kourtney",
            "khloe"
          )
        )
      }
      
      ### Royal Flush ----
      if (randomNumber11 == 5) {
        output$card1 <- renderImage({
          list(src = "10-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "10 of hearts")
        }, deleteFile = FALSE
        )
        output$card2 <- renderImage({
          list(src = "j-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Jack of hearts")
        }, deleteFile = FALSE
        )
        output$card3 <- renderImage({
          list(src = "q-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Queen of hearts")
        }, deleteFile = FALSE
        )
        output$card4 <- renderImage({
          list(src = "k-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "King of hearts")
        }, deleteFile = FALSE
        )
        output$card5 <- renderImage({
          list(src = "a-heart.png",
               width = "100%",
               height = "100%",
               contentType = "image/png", 
               alt = "Ace of hearts")
        }, deleteFile = FALSE
        )
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print("The hand that you have been dealt is a ROYAL FLUSH. 
                        Find the probability of this poker hand."),
          choices = c(
            "vanilla",
            "chocolate",
            "strawberry"
          )
        )
      }
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(png)

# Load additional dependencies and setup functions

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
<<<<<<< HEAD
          
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
          h3("Binomial Probability: Exactly vs. At Most vs. At Least"), 
          fluidRow(
            box(
              title = strong("Exactly"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 4,
              p("The probability of obtaining exactly", tags$em("r"), "events 
                in ", tags$em("n"), "trials = "), 
              p("\\(P(X=r)=\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\)"), 
              p("where ", tags$em("p"), "is the probability of success"), 
              br(), 
              br()
            ),
=======
          div(
            style = "text-align: center",
            bsButton(
              inputId = "OverviewToPrereq",
              label = "Prerequisites",
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
          tabsetPanel(
>>>>>>> d290036049001bf444b8b0689659fe7da191f892
            
            box(
              title = strong("At Most"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 4,
              p("The probability of obtaining ", tags$strong("at most"), tags$em("r"), 
                "from", tags$em("n"), "trials = "), 
              p("\\(P(X\\le{r})=\\sum_{i=0}^r\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\)"), 
              p("where ", tags$em("p"), "is the probability of success"), 
              br(), 
              br()
            ),
            
            box(
              title = strong("At Least"),
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 4,
              p("The probability of obtaining ", tags$strong("at least"), tags$em("r"), 
                "from", tags$em("n"), "trials = "),
              p("\\(P(X\\ge{r})=\\sum_{i=r}^n\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\) or"), 
              p("\\(1-P(X\\le{r})\\)"),
              p("where ", tags$em("p"), "is the probability of success")
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
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore Page
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the Concept"),
          tabsetPanel(
<<<<<<< HEAD
=======
            
            ##### CLOTHING TAB ----
>>>>>>> d290036049001bf444b8b0689659fe7da191f892
            tabPanel(
              title = "Exploring Counting Techniques",
              br(),
              fluidRow(
                column(
                  width = 2.4, 
                  offset = 1, 
                  
                  tags$figure(
                    align = "center", 
                    tags$img(
                      src = "3-heart.png", 
                      width = 50, 
                      alt = "3 of Hearts."
                    ), 
                    tags$figcaption("3 of Hearts")
                  )
                  
                ), 
                column(
                  width = 2.4, 
                  offset = 2, 
                  
                  
                  tags$figure(
                    align = "center", 
                    tags$img(
                      src = "3-heart.png", 
                      width = 50, 
                      alt = "3 of Hearts."
                    ), 
                    tags$figcaption("3 of Hearts")
                  )
                  
                ), 
                column(
                  width = 2.4, 
                  offset = 3, 
                  
                  
                  tags$figure(
                    align = "center", 
                    tags$img(
                      src = "3-heart.png", 
                      width = 50, 
                      alt = "3 of Hearts."
                    ), 
                    tags$figcaption("3 of Hearts")
                  )
                  
                ), 
                column(
                  width = 2.4, 
                  offset = 4, 
                  
                  tags$figure(
                    align = "center", 
                    tags$img(
                      src = "3-heart.png", 
                      width = 50, 
                      alt = "3 of Hearts."
                    ), 
                    tags$figcaption("3 of Hearts")
                    
                  )
                ), 
                column(
                  width = 2.4, 
                  offset = 5, 
                  tags$figure(
                    align = "center", 
                    tags$img(
                      src = "3-heart.png", 
                      width = 50, 
                      alt = "3 of Hearts."
                    ), 
                    tags$figcaption("3 of Hearts")
                    
                  )
                )
<<<<<<< HEAD
                
              ) 
=======
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
>>>>>>> d290036049001bf444b8b0689659fe7da191f892
            ),
            
            tabPanel(
              title = "Example Questions", 
              br(),
              h3("Example Problems"), 
              fluidRow(
                box(
                  title = strong("Combinations Example"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  p("There are five different types of cheese that one can  use 
                  for a cheese platter. However they can only choose three to go 
                  on the plate. How many ways can three cheeses be chosen from 
                  the five types?"), 
                  p("Answer: \\(_{5}C_{3}=\\dfrac{5!}{3!(5-3)!}=10\\)") 
                ),
                
                box(
                  title = strong("Permutations Example"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  p("Choosing from the 6 letters ABCDEF, how many ways can three 
                  letters be arranged?"), 
                  p("Answer: \\(_{6}P_{3}=\\dfrac{6!}{(6-3)!}=120\\)"), 
                  p(tags$b("Note: "),"Notice we must use the formula for permutations
                  and not combinations because ", tags$b("order matters"), "in 
                  this situation. For permutations, the group \\(ABC\\) 
                  is different from \\(CAB\\). If this was a combination
                  quesion, \\(ABC\\) and \\(CAB\\) would be considered one combination.")
                ), 
                
                box(
                  title = strong("Distinguishable Permutations Example"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  p("How many ordered arrangements are there of the word 'Bookkeeper'?"), 
                  p("Answer: \\(\\binom{10}{(1)(2)(2)(3)(1)(1)}=\\dfrac{10!}{(1!)(2!)(2!)(3!)(1!)(1!)}\\)"), #Calculate this answer
                  tags$ul(
                    tags$li("There is one letter \\(B\\)"), 
                    tags$li("There are two letter \\(O's\\)"),
                    tags$li("There are two letter \\(K's\\)"), 
                    tags$li("There are three letters \\(E's\\)"), 
                    tags$li("There is one letter \\(P\\)"), 
                    tags$li("There is one letter \\(R\\)")
                  )
                ),
                
                box(
                  title = strong("Multiplication Principle Example"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  p("If there are 9 appetizers, 12 entrees, and 5 desserts, how many
                  meals can you make?"),
                  p("Answer:", tags$em(tags$strong("Number of Meals")),"\\(=(9)\\times(12)\\times(5)=540\\)")
                  
                  
                ),
                
                box(
                  title = strong("Wording Context Change Example"),
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  p("The probability that a person has traveled out of the country 
                  is 35%. Out of the 70 people chosen:"), 
                  tags$ol(
                    tags$li("What is the probability that", tags$strong("exactly"), 
                            "21 people have traveled outside of the country?",
                            tags$ul(
                              tags$li("\\(P(X=21)=\\binom{70}{21}\\cdot(0.35^{21})
                                  \\cdot(0.65^{70-21})=0.69845\\)")
                            )),
                    br(), 
                    tags$li("What is the probability that", tags$strong("at most"), 
                            "21 people have traveled outside of the country?",
                            tags$ul(
                              tags$li("\\(P(X\\le{21})=\\sum_{i=0}^{21}\\binom{70}{i}
                                  \\cdot(0.35^{i})\\cdot(0.65^{70-i})=0.22787\\)")
                            )),
                    br(),
                    tags$li("What is the probability that", tags$strong("at least"), 
                            "21 people have traveled outside of the country?", 
                            tags$ul(
                              tags$li("\\(P(X\\ge{21})=\\sum_{i=21}^{70}\\binom{70}{i}
                                  \\cdot(0.35^{i})\\cdot(0.65^{70-i})=0.84197\\)"), 
                              #tags$li(tags$strong("OR")), 
                              tags$li("\\(1-P(X\\le{21})=\\sum_{i=0}^{20}\\binom{70}{i}
                                  \\cdot(0.35^{i})\\cdot(0.65^{70-i})=0.84197\\)")
                              
                            )),
                  )
                )
              ),
              
            )
          ),
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
  
<<<<<<< HEAD
  observeEvent(input$go1,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "prerequisites")
  })
=======
  observeEvent(
    eventExpr = input$OverviewToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites")
    }
  )
  
  observeEvent(
    eventExpr = input$PrereqToExplore,
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
      })
      output$clothesQuestion4ans <- renderUI({
        (randomNumber1+randomNumber4)
      })
      output$clothesQuestion5ans <- renderUI({
        (randomNumber1+randomNumber5)
      })
      output$clothesQuestion6ans <- renderUI({
        (randomNumber1+randomNumber6)
      })
      
    }
  )
>>>>>>> d290036049001bf444b8b0689659fe7da191f892
  
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
