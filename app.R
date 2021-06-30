# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(data.table)

# Load additional dependencies and setup functions

pqb <- fread("www/pokerquestionbank.csv")

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
          )
        ),
            
          #   ##### PROBABILITY TAB ----
          #   tabPanel(
          #     title = "Binomial Probability",
          #     br(),
          #     h3("Applying counting techniques to binomial probability"),
          #     br(),
          #     fluidRow(
          #       box(
          #         title = strong("Exactly"),
          #         status = "primary",
          #         collapsible = TRUE,
          #         collapsed = FALSE,
          #         width = 4,
          #         p("The probability of obtaining exactly", tags$em("r"), "events 
          #       in ", tags$em("n"), "trials = "), 
          #       p("\\(P(X=r)=\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\)"), 
          #       p("where ", tags$em("p"), "is the probability of success"), 
          #       br(), 
          #       br()
          #       ),
          #       
          #       box(
          #         title = strong("At Most"),
          #         status = "primary",
          #         collapsible = TRUE,
          #         collapsed = FALSE,
          #         width = 4,
          #         p("The probability of obtaining ", tags$strong("at most"), tags$em("r"), 
          #           "from", tags$em("n"), "trials = "), 
          #         p("\\(P(X\\le{r})=\\sum_{i=0}^r\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\)"), 
          #         p("where ", tags$em("p"), "is the probability of success"), 
          #         br(), 
          #         br()
          #       ),
          #       
          #       box(
          #         title = strong("At Least"),
          #         status = "primary",
          #         collapsible = TRUE,
          #         collapsed = FALSE,
          #         width = 4,
          #         p("The probability of obtaining ", tags$strong("at least"), tags$em("r"), 
          #           "from", tags$em("n"), "trials = "),
          #         p("\\(P(X\\ge{r})=\\sum_{i=r}^n\\binom{n}{r}\\cdot(p^{r})\\cdot(q^{n-r})\\) or"), 
          #         p("\\(1-P(X\\le{r})\\)"),
          #         p("where ", tags$em("p"), "is the probability of success")
          #       )
          #     )
          #   )
          # ), 
          # br(), 
          # div(
          #   style = "text-align: center",
          #   bsButton(
          #     inputId = "PrereqToExplore",
          #     label = "Explore",
          #     size = "large",
          #     icon = icon("wpexplorer"),
          #     style = "default"
          #   )
          # )
        # ),
        
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
                    tags$li("\\(\\binom{n+r-1}{r} = \\binom{24}{5} = \\dfrac{24!}{5!(19)!}\\)")
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
                    tags$li("\\(\\binom{n}{r} = \\binom{20}{5} = \\dfrac{20!}{5!(15)!}\\)")
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
          hr(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "newHand",
              label = "New Hand",
              size = "large",
              style = "default"
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              offset = 0,
              wellPanel(
                radioButtons(
                  inputId = "promptAnsOptions",
                  label = "Calculate how many ways the hand displayed can be dealt.",
                  choices =  c("Choice A","Choice B", "Choice C", "Choice D")
                ),
                uiOutput("gradingIcon")
              )
            ),
            column(
              width = 6,
              offset = 0,
                div(
                  style = "text-align: center",
                  bsButton(
                    inputId = "explainbttn",
                    label = "Answer Explanation",
                    size = "large",
                    style = "default"
                  )
                ),
                br(),
                uiOutput("explain")
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
  
  ### Defining hands and question choices ----
  
  onePair <- "\\(\\binom{13}{1}\\binom{4}{2}\\times\\binom{12}{3}\\binom{4}{1}^3\\)"
  twoPairs <- "\\(\\binom{13}{2}\\binom{4}{2}^2\\times\\binom{11}{1}\\binom{4}{1}\\)"
  threeKind <- "\\(\\binom{13}{1}\\binom{4}{3}\\times\\binom{12}{2}\\binom{4}{1}^2\\)"
  straight <- "\\(\\binom{10}{1}\\binom{4}{1}^5-\\binom{10}{1}\\binom{4}{1}\\)"
  flush <- "\\(\\binom{13}{5}\\binom{4}{1}-\\binom{10}{1}\\binom{4}{1}\\)"
  fullHouse <- "\\(\\binom{13}{1}\\binom{4}{3}\\times\\binom{12}{1}\\binom{4}{2}\\)"
  fourKind <- "\\(\\binom{13}{1}\\binom{4}{4}\\times\\binom{12}{1}\\binom{4}{1}\\)"
  straightFlush <- "\\(\\binom{10}{1}\\binom{4}{1}-\\binom{4}{1}\\)"
  royalFlush <- "\\(\\binom{1}{1}^5\\times\\binom{4}{1}\\)"
  wrongChoice1 <- "\\(\\binom{10}{1}\\times\\binom{4}{1}\\)"
  wrongChoice2 <- "\\(\\binom{10}{1}^5\\times\\binom{4}{1}\\)"
  
  observeEvent(
    eventExpr = input$newHand,
    handlerExpr = {
      randomNumber11 <- sample(1:9, 1)
      
      ### 1 Pair ----
      if (randomNumber11 == 1) {
        output$card1 <- renderUI({
          img(src = "a-club.png",
               width = "100%",
               contentType = "image/png", 
               alt = "Ace of clubs")
        })
        output$card2 <- renderUI({
          img(src = "a-heart.png",
               width = "100%",
               contentType = "image/png", 
               alt = "Ace of hearts")
        })
        output$card3 <- renderUI({
          img(src = "9-spade.png",
               width = "100%",
               contentType = "image/png", 
               alt = "9 of spades")
        })
        output$card4 <- renderUI({
          img(src = "8-diamond.png",
               width = "100%",
               contentType = "image/png", 
               alt = "8 of diamonds")
        })
        output$card5 <- renderUI({
          img(src = "7-club.png",
               width = "100%",
               contentType = "image/png", 
               alt = "7 of clubs")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[1]),
          choices = c(onePair, threeKind, straight, twoPairs)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              withMathJax(
                "For a 1 pair, we need 1 rank to appear twice: ",
                "\\(\\binom{13}{1}\\binom{4}{2}\\)",
                "For the remaining cards, they can be any rank and any suit, 
                except for the rank of the 1 pair: ",
                "\\(\\binom{12}{3}\\binom{4}{1}\\binom{4}{1}\\binom{4}{1}\\)", 
                "Therefore, the total number of ways to be dealt 1 pair is: ",
                "\\(\\binom{13}{1}\\binom{4}{2}\\times\\binom{12}{3}\\binom{4}{1}^3\\)"
              )
            )
          }
        )
      }
      
      ### 2 Pairs ----
      if (randomNumber11 == 2) {
        output$card1 <- renderUI({
          img(src = "k-diamond.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "King of diamonds")
        }
        )
        output$card2 <- renderUI({
          img(src = "k-club.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "King of clubs")
        }
        )
        output$card3 <- renderUI({
          img(src = "q-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Queen of hearts")
        })
        output$card4 <- renderUI({
          img(src = "q-spade.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Queen of spades")
        })
        output$card5 <- renderUI({
          img(src = "j-diamond.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Jack of diamonds")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[2]),
          choices = c(threeKind, twoPairs, flush, onePair)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              "For a 2 pair, we need 2 ranks to appear twice: ",
              "\\(\\binom{13}{2}\\binom{4}{2}\\binom{4}{2}\\)",
              "For the remaining card, it can be any suit and any rank, 
              except for the 2 ranks of the 2 pairs: ",
              "\\(\\binom{11}{1}\\binom{4}{1}\\)",
              "Therefore, the total number of ways to be dealt a 2 pair is: ",
              "\\(\\binom{13}{2}\binom{4}{2}^2\\times\\binom{11}{1}\\binom{4}{1}\\)"
            )
          }
        )
      }
      
      ### 3 of a kind ----
      if (randomNumber11 == 3) {
        output$card1 <- renderUI({
          img(src = "a-club.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of clubs")
        })
        output$card2 <- renderUI({
          img(src = "a-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of hearts")
        })
        output$card3 <- renderUI({
          img(src = "a-spade.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of spades")
        })
        output$card4 <- renderUI({
          img(src = "2-diamond.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "2 of diamonds")
        })
        output$card5 <- renderUI({
          img(src = "7-club.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "7 of clubs")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[3]),
          choices = c(twoPairs, straight, threeKind, flush)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              "For a 3-of-a-kind, we need 1 rank to appear three times: ",
              "\\(\\binom{13}{1}\\binom{4}{3}\\)",
              "For the remaining 2 cards, they can be any suit and any rank, 
              except for the rank of the 3-of-a-kind: ",
              "\\(\\binom{12}{2}\\binom{4}{1}\\binom{4}{1}\\)",
              "Therefore, the total number of ways to be dealt a 3-of-a-kind is: ",
              "\\(\\binom{13}{1}\\binom{4}{3}\\times\\binom{12}{2}\\binom{4}{1}^2\\)"
            )
          }
        )
      }
      
      ### Straight ----
      if (randomNumber11 == 4) {
        output$card1 <- renderUI({
          img(src = "5-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "5 of hearts")
        })
        output$card2 <- renderUI({
          img(src = "6-club.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "6 of clubs")
        })
        output$card3 <- renderUI({
          img(src = "7-diamond.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "7 of diamonds")
        })
        output$card4 <- renderUI({
          img(src = "8-spade.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "8 of spades")
        })
        output$card5 <- renderUI({
          img(src = "9-club.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "9 of clubs")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[4]),
          choices = c(threeKind, flush, twoPairs, straight)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              "For a straight, we need 5 cards in rank order of any suit. 
              Additionally, aces can be high or low, but not both simutaneously. : ",
              "\\(\\binom{10}{1}\\binom{4}{1}\\binom{4}{1}\\binom{4}{1}\\binom{4}{1}\\binom{4}{1}\\)",
              "There also exists another hand called a straight flush, 
              which is a special type of straight, so we remove these: ",
              "\\(-\\binom{10}{1}\\binom{4}{1}\\)",
              "Therefore, the total number of ways to be dealt a straight is: ",
              "\\(\\binom{10}{1}\\binom{4}{1}^5-\\binom{10}{1}\\binom{4}{1}\\)"
            )
          }
        )
      }
      
      ### Flush ----
      if (randomNumber11 == 5) {
        output$card1 <- renderUI({
          img(src = "2-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "2 of hearts")
        })
        output$card2 <- renderUI({
          img(src = "4-heart.png",
               width = "100%",
               
               contentType = "UI/png", 
               alt = "4 of hearts")
        })
        output$card3 <- renderUI({
          img(src = "6-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "6 of hearts")
        })
        output$card4 <- renderUI({
          img(src = "8-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "8 of hearts")
        })
        output$card5 <- renderUI({
          img(src = "K-heart.png",
               width = "100%",
               
               contentType = "UI/png", 
               alt = "King of hearts")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[5]),
          choices = c(flush, fourKind, straight, fullHouse)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              "For a flush, we need 5 cards of any rank, but all the same suit.
              Additionally, aces can be high or low, but not both simutaneously. : ",
              "\\(\\binom{13}{5}\\binom{4}{1}\\)",
              "There also exists another hand called a straight flush, 
              which is a special type of flush, so we remove these: ",
              "\\(-\\binom{10}{1}\\binom{4}{1}\\)",
              "Therefore, the total number of ways to be dealt a flush is: ",
              "\\(\\binom{13}{5}\\binom{4}{1}-\\binom{10}{1}\\binom{4}{1}\\)"
            )
          }
        )
      }
      
      ### Full House ----
      if (randomNumber11 == 6){
        output$card1 <- renderUI({
          img(src = "a-spade.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of spades")
        })
        output$card2 <- renderUI({
          img(src = "a-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of hearts")
        })
        output$card3 <- renderUI({
          img(src = "a-club.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of clubs")
        })
        output$card4 <- renderUI({
          img(src = "k-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "King of hearts")
        })
        output$card5 <- renderUI({
          img(src = "k-spade.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "King of spades")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[6]),
          choices = c(straight, fullHouse, flush, fourKind)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              "For a full house, we need 3 cards of the same rank and 2 cards of the same rank, 
              but different from that of the first 3 cards. all cards can be of any suit. Simply,
              think of combining a 3-of-a-kind and a 1 pair into the same hand.",
              "3-of-a-kind: ", "\\(\\binom{13}{1}\\binom{4}{3}\\)",
              "1 pair: ", "\\(\\binom{13}{1}\\binom{4}{2}\\)",
              "Therefore, the total number of ways you can be dealt a full house is: ",
              "\\(\\binom{13}{1}\\binom{4}{3}\\times\\binom{12}{1}\\binom{4}{2}\\)"
            )
          }
        )
      }
      
      ### 4 of a kind ----
      if (randomNumber11 == 7){
        output$card1 <- renderUI({
          img(src = "a-spade.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of spades")
        })
        output$card2 <- renderUI({
          img(src = "a-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of hearts")
        })
        output$card3 <- renderUI({
          img(src = "a-club.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of clubs")
        })
        output$card4 <- renderUI({
          img(src = "a-diamond.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of diamonds")
        })
        output$card5 <- renderUI({
          img(src = "2-spade.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "2 of spades")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[7]),
          choices = c(fullHouse, straightFlush, fourKind, flush)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              "For a 4-of-a-kind, we need 4 cards of the same suit and any rank: ",
              "\\(\\binom{13}{1}\\binom{4}{4}\\)",
              "The last remaining card, can be any suit and any rank, 
              except for the rank used in the 4-of-a-kind: ",
              "\\(\\binom{12}{1}\\binom{4}{1}\\)",
              "Therefore, the total number of ways you can be dealt a 4-of-a-kind is: ",
              "\\(\\binom{13}{1}\\binom{4}{4}\\times\\binom{12}{1}\\binom{4}{1}\\)"
            )
          }
        )
      }
      
      ### Straight Flush ----
      if (randomNumber11 == 8) {
        output$card1 <- renderUI({
          img(src = "5-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "5 of hearts")
        })
        output$card2 <- renderUI({
          img(src = "6-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "6 of hearts")
        })
        output$card3 <- renderUI({
          img(src = "7-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "7 of hearts")
        })
        output$card4 <- renderUI({
          img(src = "8-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "8 of hearts")
        })
        output$card5 <- renderUI({
          img(src = "9-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "9 of hearts")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[8]),
          choices = c(fourKind, royalFlush, wrongChoice1, straightFlush)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              "For a straight flush, we need 5 cards in rank order of the same suit. 
              Additionally, aces can be high or low, but not both simutaneously: ",
              "\\(\\binom{10}{1}\\binom{4}{1}\\)",
              "There also exists another hand called a royal flush, 
              which is a special type of straight flush, so we remove these: ",
              "\\(-\\binom{1}{1}^5\\times\\binom{4}{1}\\)",
              "Therefore, the total number of ways to be dealt a straight flush is: ",
              "\\(\\binom{10}{1}\\binom{4}{1}-\\binom{4}{1}\\)"
            )
          }
        )
      }
      
      ### Royal Flush ----
      if (randomNumber11 == 5) {
        output$card1 <- renderUI({
          img(src = "10-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "10 of hearts")
        })
        output$card2 <- renderUI({
          img(src = "j-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Jack of hearts")
        })
        output$card3 <- renderUI({
          img(src = "q-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Queen of hearts")
        })
        output$card4 <- renderUI({
          img(src = "k-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "King of hearts")
        })
        output$card5 <- renderUI({
          img(src = "a-heart.png",
               width = "100%",
               contentType = "UI/png", 
               alt = "Ace of hearts")
        })
        updateRadioButtons(
          session = session,
          inputId = "promptAnsOptions",
          label = print(pqb$question[9]),
          choices = c(royalFlush, straightFlush, wrongChoice1, wrongChoice2)
        )
        output$math1 <- renderUI({withMathJax()})
        output$math2 <- renderUI({withMathJax()})
        observeEvent(
          eventExpr = input$explainbttn,
          handlerExpr = {
            output$explain <- renderUI(
              "For a royal flush, we need a straight flush but with only the 5 
              top ranking cards (Ace, King, Queen, Jack, and 10): ",
              "\\(\\binom{1}{1}^5\\times\\binom{4}{1}\\)",
              "Therefore, the total number of ways you can be dealt a royal flush is: ",
              "\\(\\binom{4}{1}\\)"
            )
          }
        )
      }
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
