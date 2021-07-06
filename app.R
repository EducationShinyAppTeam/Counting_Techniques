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
            ##### Tab Panel I'm Working On ----
            tabPanel(
              withMathJax(), 
              title = "Different Scenarios", 
              br(),
              h3("Explore Different Scenarios"), 
              p("Context 2: "), 
              tags$ul(
                p("a"), 
                tags$li("Answer: \\({3}\\cdot{5}\\cdot{(4+1)}\\)"), ## Answer
                tags$li("\\({3}\\cdot{5}\\cdot{4}\\)"), 
                tags$li("\\([{3}\\cdot{5}]+[{3}\\cdot{5}\\cdot{(4+1)}]\\)"), 
                tags$li("\\([{3}\\cdot{5}]+[{3}\\cdot{5}\\cdot{4}]\\)")
              ), 
              tags$ul(
                p("b"), 
                tags$li("\\([(2^3-1)\\cdot(2^5-1)\\cdot{2^4}] +
                        [3\\cdot(_{5}C_{1}\\cdot_{5}C_{2})\\cdot{2^4}]\\)"), 
                tags$li("\\([(2^3)\\cdot(2^5)\\cdot{2^4}] +
                        [3\\cdot(_{5}C_{1}+_{5}C_{2})\\cdot{2^4}]\\)"), 
                tags$li("Answer: \\([(2^3-1)\\cdot(2^5-1)\\cdot{2^4}] +
                        [3\\cdot(_{5}C_{1}+_{5}C_{2})\\cdot{2^4}]\\)"), ## Answer
                tags$li("\\([{3}\\cdot{5}\\cdot{4}]+[_{3}C_{1}\\cdot(_{5}C_{1}+_{5}C_{2})\\cdot{4}]\\)")
              ), 
              tags$ul(
                p("c"), 
                tags$li("Answer: \\({3}\\cdot{5}\\cdot{(4+1)}\\)"), ## Answer
                tags$li("\\({3}\\cdot{5}\\cdot{4}\\)"), 
                tags$li("\\({1}\\cdot{2}\\cdot{(4+1)}\\)"), 
                tags$li("\\({1}\\cdot{(_{5}C_{1}+_{5}C_{2})}\\cdot{(4+1)}\\)")
              ), 
              
              p("Context 3: "), 
              tags$ul(
                p("a"),
                tags$li("\\({5}\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3}\\)"), 
                tags$li("Answer: \\(5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3}\\)"), ## Answer
                tags$li("\\({5}\\cdot{15}\\cdot{3}\\)"), 
                tags$li("\\(5\\cdot[_{3}C_{2}\\cdot{3}\\cdot(3\\cdot{4})]\\cdot{3}\\)")
              ), 
              
              tags$ul(
                p("b"),
                tags$li("\\(({5}\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})^5\\)"), 
                tags$li("\\(({5}\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})\\cdot{5}\\)"), 
                tags$li("\\((5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3})\\cdot{5}\\)"), 
                tags$li("Answer: \\((5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3})^5\\)") ## Answer
              ), 
              
              tags$ul(
                p("c"),
                tags$li("\\(\\dfrac{({5}\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})!}
                        {(({5}\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})-5)!}\\)"), 
                tags$li("Answer: \\(\\dfrac{(5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3})!}
                        {([5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3}] - 5)!}\\)"), ##Answer
                tags$li("\\(\\dfrac{({5}\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})!}
                        {{5!}(({5}\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})-5)!}\\)"), 
                tags$li("\\(\\dfrac{(5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3})!}
                        {([5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3}]\\cdot{5})!}\\)")
              ), 
              
              tags$ul(
                p("d"),
                tags$li("\\(\\dfrac{(5\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})!}
                        {5![(5\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})-5]!}\\)"), 
                tags$li("Answer: \\(\\dfrac{(5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3})!}
                        {5!(5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})]\\cdot{3})-5)!}\\)"), ##Answer
                tags$li("\\(\\dfrac{5\\cdot[_{3}C_{2}+3(3\\cdot{4}]\\cdot{3})!}
                        {(5\\cdot[_{3}C_{2}+3\\cdot(3\\cdot{4})\\cdot{3})-5)!}\\)"), 
                tags$li("\\(\\dfrac{(5\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})!}
                        {[(5\\cdot{3}\\cdot{3}\\cdot{4}\\cdot{3})-5]!}\\)"), 
                
              ), 
              
              tags$ul(
                p("e"),
                tags$li("Answer: \\(2^5\\)"), ##Answer
                tags$li("\\(2\\cdot{2}\\cdot{1}\\)"), 
                tags$li("\\(_{5}C_{1}\\cdot{5}\\)"), 
                tags$li("\\(2^5+2^5+1^5\\)")
              ), 
              
              p("Context 4: "), 
              tags$ul(
                p("a"), 
                tags$li("\\(_{4+3+4}C_{1}\\)"), 
                tags$li("\\((4^3)^4\\)"), 
                tags$li("Answer: \\({4}\\cdot{3}\\cdot{4}\\)"), ## Answer
                tags$li("\\(\\dfrac{(4\\cdot{3}\\cdot{4})!}{4!3!4!}\\)")
              ), 
              tags$ul(
                p("b"), 
                tags$li("\\(4(2+_{6}C_{1})\\)"), 
                tags$li("\\(4(2+_{4}C_{1})\\)"), 
                tags$li("Answer: \\([_{12}C_{3}+_{12}C_{2}\\cdot2]\\cdot{4^3}\\)"), ## Answer
                tags$li("\\((2+_{4}C_{1})\\cdot{4^3}\\)")
              ), 
              
              p("Context 5: "), 
              tags$ul(
                p("a"), 
                tags$li("\\(15^4\\)"), 
                tags$li("\\(4!\\)"), 
                tags$li("\\(_{15}P_{4}\\) or \\(\\dfrac{15!}{(15-4)!}\\)"), 
                tags$li("Answer: \\(_{15}C_{4}\\) or \\(\\dfrac{15!}{4!(15-4)!}\\)") ## Answer
              ), 
              tags$ul(
                p("b"), 
                tags$li("\\(4(1+_{14}C_{3})\\)"), 
                tags$li("Answer: \\(15\\cdot_{14}C_{3} = _{15}C_{4}\\cdot{4})\\)"), ## Answer
                tags$li("\\(4\\cdot(15^4)\\)"), 
                tags$li("\\(4(_{4}C_{1} + _{11}C_{3})\\)")
              ), 
              
              p("Context 6: "), 
              tags$ul(
                p("a"), 
                tags$li("\\(6\\cdot_{50}C_{1}\\)"), 
                tags$li("\\(6\\cdot_{50}C_{6}\\)"), 
                tags$li("\\(300^6\\)"), 
                tags$li("Answer: \\(_{300}C_{6}\\)") ## Answer
              ), 
              tags$ul(
                p("b"), 
                tags$li("\\(6\\cdot_{50}C_{1}\\)"), 
                tags$li("\\(\\dfrac{_{300}C_{6}}{6}\\)"), 
                tags$li("Answer: \\(6\\cdot_{50}C_{6}\\)"), ## Answer
                tags$li("\\(50!\\cdot{49!}\\cdot{48}!\\cdot{47!}\\cdot{46!}\\)")
              ), 
              tags$ul(
                p("c"), 
                tags$li("\\(50\\cdot{49}\\cdot{48}\\cdot{47}+350\\cdot{349}\\)"), 
                tags$li("\\([6\\cdot(_{50}C_{4})]\\cdot[_{350}C_{2}]\\)"), 
                tags$li("\\([6\\cdot(_{300}C_{4})]\\cdot[_{296}C_{2}]\\)"), 
                tags$li("Answer: \\([6\\cdot(_{50}C_{4})]\\cdot[_{5}C_{2}\\cdot{50}\\cdot{50}]\\)") ## Answer
              ), 
              
              tags$ul(
                p("d"), 
                tags$li("Answer: \\([6\\cdot{_{50}C_{4}}][_{5}C_{2}\\cdot{50}\\cdot{50}]+[6\\cdot{_{50}C_{5}}][5\\cdot{50}]+[6\\cdot{_{50}C_{6}}]\\)"), ##Answer
                tags$li("\\([_{50}C_{4}][_{5}C_{2}\\cdot{50}\\cdot{50}]+[_{50}C_{5}][5\\cdot{50}]+[_{50}C_{6}]\\)"), 
                tags$li("\\([6\\cdot{_{50}C_{4}}][_{5}C_{2}\\cdot{50}\\cdot{50}]\\cdot[6\\cdot{_{50}C_{5}}][5\\cdot{50}]\\cdot[6\\cdot{_{50}C_{6}}]\\)"), 
                tags$li("\\([_{50}C_{4}][_{5}C_{2}\\cdot{50}\\cdot{50}]\\cdot[_{50}C_{5}][5\\cdot{50}]\\cdot[_{50}C_{6}]\\)"), 
              ), 
              
              tags$ul(
                p("e"), 
                tags$li("\\(_{300}C_{6}+([6\\cdot{_{50}C_{4}}][_{5}C_{2}\\cdot{50}\\cdot{50}]+[6\\cdot{_{50}C_{5}}][5\\cdot{50}]+[6\\cdot{_{50}C_{6}}])\\)"), ##Answer
                tags$li("\\(_{300}C_{6}-6\\cdot(50\\cdot{49}\\cdot{48}\\cdot{47})\\)"), 
                tags$li("Answer: \\(_{300}C_{6}–([6\\cdot{_{50}C_{4}}][_{5}C_{2}\\cdot{50}\\cdot{50}]+[6\\cdot{_{50}C_{5}}][5\\cdot{50}]+[6\\cdot{_{50}C_{6}}])\\)"), ##Answer
                tags$li("\\([6\\cdot{_{50}C_{4}}][_{5}C_{2}\\cdot{50}\\cdot{50}]+[6\\cdot{_{50}C_{5}}][5\\cdot{50}]+[6\\cdot{_{50}C_{6}}]\\)"), 
              )
              
            ), 
            
            tabPanel(
              withMathJax(),
              title = "Multiple Choice", 
              
              fluidRow(
                column(
                  width = 12,
                  wellPanel(
                    style = "background-color: #FFFFFF",
                    
                    h4("Context"),
                    uiOutput("context"), 
                    br(), 
                    h4("Question:"), 
                    uiOutput("question"),
                    br(),
                    bsButton(
                      inputId = "hint",
                      label = "Hint",
                      icon = icon("question"),
                      size = "large", 
                      disabled = FALSE
                    ),
                    br(),
                    radioGroupButtons(
                      inputId = "mc1",
                      label = "Which expression addresses the question?",
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
                      width = "100%",
                      justified = FALSE,
                      individual = FALSE
                      
                      
                    ),
                    
                    #Paste hint instead of pop-up 
                    uiOutput("hintDisplay"),
                    br(),
                    
                    fluidRow(
                      column(
                        width = 3,
                        bsButton(
                          inputId = "submit",
                          label = "Submit",
                          size = "large",
                          style = "default",
                          disabled = FALSE
                        )
                      ),
                      column(
                        width = 4,
                        uiOutput("mark")
                      )
                    ),
                    br(),
                    bsButton(
                      inputId = "nextq",
                      label = "Next Question",
                      size = "large",
                      style = "default",
                      disabled = TRUE
                    ),
                    br(),
                    bsButton(
                      "restart",
                      "Restart",
                      size = "large",
                      style = "danger",
                      disabled = FALSE
                    )
                  )
                )
              ),
              uiOutput("math1"),
              uiOutput("math2")
            ), 
            # tabPanel(
            #   withMathJax(),
            #   title = "Excel Sheet",
            #   br(),
            #   h3("Explore Different Scenarios"),
            #   p("Context 2: "),
            #   tags$ul(
            #     p("a"),
            #     tags$li("Answer: \({3}\cdot{5}\cdot{(4+1)}\)"), ## Answer
            #     tags$li("\({3}\cdot{5}\cdot{4}\)"),
            #     tags$li("\([{3}\cdot{5}]+[{3}\cdot{5}\cdot{(4+1)}]\)"),
            #     tags$li("\([{3}\cdot{5}]+[{3}\cdot{5}\cdot{4}]\)")
            #   ),
            #   tags$ul(
            #     p("b"),
            #     tags$li("\([(2^3-1)\cdot(2^5-1)\cdot{2^4}] +
            #             [3\cdot(_{5}C_{1}\cdot_{5}C_{2})\cdot{2^4}]\)"),
            #     tags$li("\([(2^3)\cdot(2^5)\cdot{2^4}] +
            #             [3\\cdot(_{5}C_{1}+_{5}C_{2})\cdot{2^4}]\)"),
            #     tags$li("Answer: \([(2^3-1)\cdot(2^5-1)\cdot{2^4}] +
            #             [3\cdot(_{5}C_{1}+_{5}C_{2})\cdot{2^4}]\)"), ## Answer
            #     tags$li("\([{3}\cdot{5}\cdot{4}]+[_{3}C_{1}\cdot(_{5}C_{1}+_{5}C_{2})\cdot{4}]\)")
            #   ),
            #   tags$ul(
            #     p("c"),
            #     tags$li("Answer: \({3}\cdot{5}\cdot{(4+1)}\)"), ## Answer
            #     tags$li("\({3}\cdot{5}\cdot{4}\)"),
            #     tags$li("\({1}\cdot{2}\cdot{(4+1)}\)"),
            #     tags$li("\({1}\cdot{(_{5}C_{1}+_{5}C_{2})}\cdot{(4+1)}\)")
            #   ),
            # 
            #   p("Context 3: "),
            #   tags$ul(
            #     p("a"),
            #     tags$li("\({5}\cdot{3}\cdot{3}\cdot{4}\cdot{3}\)"),
            #     tags$li("Answer: \(5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3}\)"), ## Answer
            #     tags$li("\({5}\cdot{15}\cdot{3}\)"),
            #     tags$li("\(5\cdot[_{3}C_{2}\cdot{3}\cdot(3\cdot{4})]\cdot{3}\)")
            #   ),
            # 
            #   tags$ul(
            #     p("b"),
            #     tags$li("\(({5}\cdot{3}\cdot{3}\cdot{4}\cdot{3})^5\)"),
            #     tags$li("\(({5}\cdot{3}\cdot{3}\cdot{4}\cdot{3})\cdot{5}\)"),
            #     tags$li("\((5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3})\cdot{5}\)"),
            #     tags$li("Answer: \((5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3})^5\)") ## Answer
            #   ),
            # 
            #   tags$ul(
            #     p("c"),
            #     tags$li("\(\dfrac{({5}\cdot{3}\cdot{3}\cdot{4}\cdot{3})!}
            #             {(({5}\cdot{3}\cdot{3}\cdot{4}\cdot{3})-5)!}\)"),
            #     tags$li("Answer: \(\dfrac{(5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3})!}
            #             {([5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3}] - 5)!}\)"), ##Answer
            #     tags$li("\(\dfrac{({5}\cdot{3}\cdot{3}\cdot{4}\cdot{3})!}
            #             {{5!}(({5}\cdot{3}\cdot{3}\cdot{4}\cdot{3})-5)!}\)"), 
            #     tags$li("\(\dfrac{(5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3})!}
            #             {([5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3}]\cdot{5})!}\)")
            #   ), 
            # 
            # tags$ul(
            #   p("d"),
            #   tags$li("\(\dfrac{(5\cdot{3}\cdot{3}\cdot{4}\cdot{3})!}
            #             {5![(5\cdot{3}\cdot{3}\cdot{4}\cdot{3})-5]!}\)"), 
            #   tags$li("Answer: \(\dfrac{(5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3})!}
            #             {5!(5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})]\cdot{3})-5)!}\)"), ##Answer
            #   tags$li("\(\dfrac{5\cdot[_{3}C_{2}+3(3\cdot{4}]\cdot{3})!}
            #             {(5\cdot[_{3}C_{2}+3\cdot(3\cdot{4})\cdot{3})-5)!}\)"), 
            #   tags$li("\(\dfrac{(5\cdot{3}\cdot{3}\cdot{4}\cdot{3})!}
            #             {[(5\cdot{3}\cdot{3}\cdot{4}\cdot{3})-5]!}\)"), 
            #   ),
            # 
            #   tags$ul(
            #     p("e"),
            #     tags$li("Answer: \(2^5\)"), ##Answer
            #     tags$li("\(2\cdot{2}\cdot{1}\)"),
            #     tags$li("\(_{5}C_{1}\cdot{5}\)"),
            #     tags$li("\(2^5+2^5+1^5\)")
            #   ),
            # 
            #   p("Context 4: "),
            #   tags$ul(
            #     p("a"),
            #     tags$li("\(_{4+3+4}C_{1}\)"),
            #     tags$li("\((4^3)^4\)"),
            #     tags$li("Answer: \({4}\cdot{3}\cdot{4}\)"), ## Answer
            #     tags$li("\(\dfrac{(4\cdot{3}\cdot{4})!}{4!3!4!}\)")
            #   ),
            #   tags$ul(
            #     p("b"),
            #     tags$li("\(4(2+_{6}C_{1})\)"),
            #     tags$li("\(4(2+_{4}C_{1})\)"),
            #     tags$li("Answer: \([_{12}C_{3}+_{12}C_{2}\cdot2]\cdot{4^3}\)"), ## Answer
            #     tags$li("\((2+_{4}C_{1})\cdot{4^3}\)")
            #   ),
            # 
            #   p("Context 5: "),
            #   tags$ul(
            #     p("a"),
            #     tags$li("\(15^4\)"),
            #     tags$li("\(4!\)"),
            #     tags$li("\(_{15}P_{4}\) or \(\dfrac{15!}{(15-4)!}\)"),
            #     tags$li("Answer: \(_{15}C_{4}\) or \(\dfrac{15!}{4!(15-4)!}\)") ## Answer
            #   ),
            #   tags$ul(
            #     p("b"),
            #     tags$li("\(4(1+_{14}C_{3})\)"),
            #     tags$li("Answer: \(15\cdot_{14}C_{3} = _{15}C_{4}\cdot{4})\)"), ## Answer
            #     tags$li("\(4\cdot(15^4)\)"),
            #     tags$li("\(4(_{4}C_{1} + _{11}C_{3})\)")
            #   ),
            # 
            #   p("Context 6: "),
            #   tags$ul(
            #     p("a"),
            #     tags$li("\(6\cdot_{50}C_{1}\)"),
            #     tags$li("\(6\cdot_{50}C_{6}\)"),
            #     tags$li("\(300^6\)"),
            #     tags$li("Answer: \(_{300}C_{6}\)") ## Answer
            #   ),
            #   tags$ul(
            #     p("b"),
            #     tags$li("\(6\cdot_{50}C_{1}\)"),
            #     tags$li("\(\dfrac{_{300}C_{6}}{6}\)"),
            #     tags$li("Answer: \(6\cdot_{50}C_{6}\)"), ## Answer
            #     tags$li("\(50!\cdot{49!}\cdot{48}!\cdot{47!}\cdot{46!}\)")
            #   ),
            #   tags$ul(
            #     p("c"),
            #     tags$li("\(50\cdot{49}\cdot{48}\cdot{47}+350\cdot{349}\)"),
            #     tags$li("\([6\cdot(_{50}C_{4})]\cdot[_{350}C_{2}]\)"),
            #     tags$li("\([6\cdot(_{300}C_{4})]\cdot[_{296}C_{2}]\)"),
            #     tags$li("Answer: \([6\cdot(_{50}C_{4})]\cdot[_{5}C_{2}\cdot{50}\cdot{50}]\)") ## Answer
            #   ),
            # 
            # tags$ul(
            #   p("d"), 
            #   tags$li("Answer: \([6\cdot{_{50}C_{4}}][_{5}C_{2}\cdot{50}\cdot{50}]+[6\cdot{_{50}C_{5}}][5\cdot{50}]+[6\cdot{_{50}C_{6}}]\)"), ##Answer
            #   tags$li("\([_{50}C_{4}][_{5}C_{2}\cdot{50}\cdot{50}]+[_{50}C_{5}][5\cdot{50}]+[_{50}C_{6}]\)"), 
            #   tags$li("\([6\cdot{_{50}C_{4}}][_{5}C_{2}\cdot{50}\cdot{50}]\cdot[6\cdot{_{50}C_{5}}][5\cdot{50}]\cdot[6\cdot{_{50}C_{6}}]\)"), 
            #   tags$li("\([_{50}C_{4}][_{5}C_{2}\cdot{50}\cdot{50}]\cdot[_{50}C_{5}][5\cdot{50}]\cdot[_{50}C_{6}]\)"), 
            # ), 
            # 
            # tags$ul(
            #   p("e"), 
            #   tags$li("\(_{300}C_{6}+([6\cdot{_{50}C_{4}}][_{5}C_{2}\cdot{50}\cdot{50}]+[6\cdot{_{50}C_{5}}][5\cdot{50}]+[6\cdot{_{50}C_{6}}])\)"), 
            #   tags$li("\(_{300}C_{6}-6\cdot(50\cdot{49}\cdot{48}\cdot{47})\)"), 
            #   tags$li("Answer: \(_{300}C_{6}–([6\\cdot{_{50}C_{4}}][_{5}C_{2}\cdot{50}\cdot{50}]+[6\cdot{_{50}C_{5}}][5\cdot{50}]+[6\cdot{_{50}C_{6}}])\)"), ##Answer
            #   tags$li("\([6\cdot{_{50}C_{4}}][_{5}C_{2}\cdot{50}\cdot{50}]+[6\cdot{_{50}C_{5}}][5\cdot{50}]+[6\cdot{_{50}C_{6}}]\)"), 
            # ), 
            
            # 
            # ),
            
            
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
                              
                            ))
                  )
                )
              )
              
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
  ##### Variables starting value---- 
  selected <<- c()
  correct_answer <<- c()
  
  # withBusyIndicatorServer <- function(buttonId, expr) {
  #   # UX stuff: show the "busy" message, hide the other messages, disable the button
  #   loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  #   doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  #   errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  #   shinyjs::disable(buttonId)
  #   shinyjs::show(selector = loadingEl)
  #   shinyjs::hide(selector = doneEl)
  #   shinyjs::hide(selector = errEl)
  #   on.exit({
  #     shinyjs::enable(buttonId)
  #     shinyjs::hide(selector = loadingEl)
  #   })
  #   
  #   # Try to run the code when the button is clicked and show an error message if
  #   # an error occurs or a success message if it completes
  #   tryCatch({
  #     value <- expr
  #     shinyjs::show(selector = doneEl)
  #     shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
  #                                        time = 0.5))
  #     value
  #   }, error = function(err) { errorFunc(err, buttonId) })
  # }
  
  ##### Reading in Questions ----
  bank <- read.csv("exploreQuestions.csv", stringsAsFactors = FALSE)
  Qs_array <- c(1:nrow(bank))
  value <- reactiveValues(
    index = 1,
    mistake = 0,
    correct = 0
  )
  #hint <- as.matrix(bank[1:nrow(bank), 10])
  
  # Reset button
  observeEvent(input$restart, {
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
    
    
    Qs <<- nrow(bank)
    Qs_array <<- c(1:Qs)
    id <- 1
    
    GAME_OVER <<- FALSE
    
    output$question <- renderUI({
      withMathJax()
      hint <<- withMathJax(bank[id, 10])
      return(bank[id, 5])
    })
    
    output$context <- renderUI({
      withMathJax()
      hint <<- withMathJax(bank[id, 10])
      return(bank[id, 4])
    })
    
    output$hint <- renderUI({
      withMathJax()
      hint <<- withMathJax(bank[id, 10])
      return(bank[id, 10])
    })
    
    updateRadioGroupButtons(
      session, "mc1",
      choices = list(
        "A" = bank[id, "A"],
        "B" = bank[id, "B"],
        "C" = bank[id, "C"],
        "D" = bank[id, "D"] 
      ),
      selected = character(0),
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
    value[["mistake"]] <<- 0
    value$correct <<- 0
  })
  
  # Print out a question
  output$question <- renderUI({
    id <<- sample(Qs_array, 1, replace = FALSE, prob = NULL)
    Qs_array <<- Qs_array[!Qs_array %in% id]
    updateRadioGroupButtons(
      session, "mc1",
      selected = character(0),
      choices = list(
        bank[id, "A"],
        bank[id, "B"],
        bank[id, "C"],
        bank[id, "D"]
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
    hint <<- withMathJax(bank[id, 10])
    return(withMathJax(bank[id, 5]))
  })
  
  output$context <- renderUI({
    withMathJax()
    hint <<- withMathJax(bank[id, 10])
    return(bank[id, 4])
  })
  
  ### NEXT QUESTION BUTTON###
  observeEvent(input$nextq, {
    if (length(Qs_array) > 1) {
      id <<- sample(Qs_array, 1, replace = FALSE, prob = NULL)
      Qs_array <<- Qs_array[!Qs_array %in% id]
      hint <<- bank[id, 10]
      withBusyIndicatorServer("nextq", {
        updateButton(session, "submit", disabled = FALSE)
        output$question <- renderUI({
          return(withMathJax(bank[id, 5]))
        })
        output$context <- renderUI({
          return(withMathJax(bank[id, 4]))
        })
        updateRadioGroupButtons(
          session, "mc1",
          selected = character(0),
          choices = list(
            "A" = bank[id, "A"],
            "B" = bank[id, "B"],
            "C" = bank[id, "C"],
            "D" = bank[id, "D"] 
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
    }
    else if (length(Qs_array) == 1) {
      id <<- Qs_array[1]
      Qs_array <<- Qs_array[!Qs_array %in% id]
      hint <<- bank[id, 10]
      withBusyIndicatorServer("nextq", {
        output$question <- renderUI({
          return(withMathJax(bank[id, 5]))
        })
        output$context <- renderUI({
          return(withMathJax(bank[id, 4]))
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
            "A" = bank[id, "A"],
            "B" = bank[id, "B"],
            "C" = bank[id, "C"],
            "D" = bank[id, "D"] 
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
      output$context <- renderUI({
        return(NULL)
      })
      output$hintDisplay <- renderUI({
        return(NULL)
      })
      updateRadioGroupButtons(
        session, "mc1",
        selected = character(0),
        choices = list(
          bank[id, "A"],
          bank[id, "B"],
          bank[id, "C"],
          bank[id, "D"]
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
    cAnswer <- bank[id, "Answer"]
    print(cAnswer)
    print(input$mc1)
    WIN <- FALSE
    if(!is.null(input$mc1) || length(input$mc1) != 0){
      success <- input$mc1 == cAnswer
    } else {
      success <- FALSE
    }
    
    if (success) {
      # print("correct")
      value$correct <- value$correct + 1
      if (value$correct == 12) {
        WIN <- TRUE
        GAME_OVER <<- TRUE
        sendSweetAlert(
          session = session,
          title = "Success:",
          type = "success",
          closeOnClickOutside = TRUE,
          h4("Congrats! You Win! Please click Restart to start over.")
        )
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
        # output$hintDisplay <- renderUI({
        #   return(NULL)
        # })
      }
      else {
        updateButton(
          session = session, 
          inputId = "submit", 
          disabled = TRUE)
        updateButton(
          session = session, 
          inputId = "nextq", 
          disabled = FALSE)
        
        
      }
    } else {
      # print("wrong")
      value[["mistake"]] <<- value[["mistake"]] + 1
      if (value[["mistake"]] == 4) {
        WIN <- FALSE
        GAME_OVER <<- TRUE
        sendSweetAlert(
          session = session,
          title = "Lost:",
          type = "error",
          closeOnClickOutside = TRUE,
          h4("You lost. Please click Restart to start over")
        )
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
        
      } else {
        updateButton(
          session = session, 
          inputId = "submit", 
          disabled = TRUE)
        updateButton(
          session = session, 
          inputId = "nextq",
          disabled = FALSE)
      }
    }
    
    
    output$mark <- renderUI({
      if(!is.null(input$mc1) || length(input$mc1) != 0) {
        if (input$mc1 == cAnswer) {
          img(src = "check.png", width = 50, alt = "Correct.")
        } else {
          img(src = "cross.png", width = 50, alt = "Incorrect")
        }
      } else {
        img(src = "cross.png", width = 50, alt = "Incorrect")
      }
    })
  })
  
  #### PRINT NUMBER OF CORRECT ANSWERS####
  output$correct <- renderUI({
    paste("Number of correct answers:", value$correct)
  })
  
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
        p(tags$b("Hint:"), bank[id, 10])
      })
    })
  
}
# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)