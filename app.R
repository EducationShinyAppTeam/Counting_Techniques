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
                    inputId = "new_wardrobe",
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
              ),
              box(
                title = strong("Permutation without Replacement"),
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 6,
                p("How many outfits could you make with the given wardrobe, 
                  without rewearing any clothing articles?")
              )
            ),
            fluidRow(
              box(
                title = strong("Combination with Replacement"),
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 6,
                p("How many different outfits could you make with the given wardrobe?")
              ),
              box(
                title = strong("Combination without Replacement"),
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 6,
                p("How many different outfits could you make with the given wardrobe, 
              without rewearing any clothing articles?")
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
                  how many different outfits could you make with the given wardrobe?")
              ),
              box(
                title = strong("Conditional combination without Replacement"),
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 6,
                p("Given that there is warm weather, 
                  how many different outfits could you make with the given wardrobe, 
                  without rewearing any clothing articles?")
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
          )
          )
        ),
        
        #### Game/Poker Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Poker Probability"),
          inputPanel(
            column(
              width = 2,
              offset = 1,
              align = "center",
              tags$img(
                src = "question.png",
                alt = "Undetermined poker card",
                width = "100%"
              ),
              tags$img(
                src = "question.png",
                alt = "Undetermined poker card",
                width = "100%"
              ),
              tags$img(
                src = "question.png",
                alt = "Undetermined poker card",
                width = "100%"
              ),
              tags$img(
                src = "question.png",
                alt = "Undetermined poker card",
                width = "100%"
              ),
              tags$img(
                src = "question.png",
                alt = "Undetermined poker card",
                width = "100%"
              )
            )
          )
          ),

        #### Set up the References Page ----
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
  
  
  observeEvent(
    eventExpr = input$new_wardrobe,
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
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
