library(shiny)
library(shinymaterial)
library(shinydashboard)
library(plotly)
library(DT)

# flow: input -> box group -> tab -> full ui


# global var --------------------------------------------------------------


methodList <- list("Predictive mean matching" = "pmm",
                   "Weighted predictive mean matching" = "midastouch",
                   "Random sample from observed values" = "sample",
                   "Classification and regression trees" = "cart",
                   "Random forest imputations" = "rf",
                   "Unconditional mean imputation" = "mean",
                   "Bayesian linear regression" = "norm",
                   "Linear regression ignoring model error" = "norm.nob",
                   "Linear regression using bootstrap" = "norm.boot",
                   "predicted values" = "norm.predict",
                   "Imputation of quadratic terms" = "quadratic",
                   "Random indicator for nonignorable data" = "ri",
                   "Level-1 normal heteroskedastic" = "2l.norm",
                   "Level-1 normal homoscedastic, lmer" = "2l.lmer",
                   "Level-1 normal homoscedastic, pan" = "2l.pan",
                   "Level-2 class mean" = "2lonly.mean",
                   "Level-2 class normal" = "2lonly.norm",
                   "Level-2 class predictive mean matching" = "2lonly.pmm")



# inputs lab --------------------------------------------------------------


inMissType <- material_radio_button(
  input_id = "missType",
  label = "Missingness Mechanism:", 
  choices = c("MCAR", "MAR", "MNAR1", "MNAR2")
)

inMissPercent <- material_slider(
  input_id = "missPercent", 
  "% missing data",
  min_value = 1, max_value = 99, initial_value = 20
)

inImputeM <- material_dropdown(
  input_id = "imputeMethod",
  label = "Imputation Mechanism:", 
  choices = as.character(methodList)
)

inImputeMulti <- material_number_box(
  input_id = "imputeN",
  label = "# multi-imuptation",
  min_value = 2, max_value = 30, initial_value = 10
)

runImputation <- material_button(
  input_id = "runImp",
  label = "Run Imputation"
)

inShowGroup <- material_switch(
  input_id = "showGroup", 
  label = " group by age",
  initial_value = TRUE
)

inPlotItems <- material_dropdown(
  input_id = "plotItems", 
  label = "Plot Contents:",
  multiple = TRUE,
  choices = c("")
)



# inputs em ---------------------------------------------------------------


inMu1 <- material_row(
  material_column(width = 4, material_number_box("mean1x", "Mean x", initial_value = 3, min_value = -30, max_value = 30)),
  material_column(width = 4, material_number_box("mean1y", "y", initial_value = 3, min_value = -30, max_value = 30)),
  material_column(width = 4, material_number_box("mean1z", "z", initial_value = 3, min_value = -30, max_value = 30))
)


inSigma1 <- material_row(
  material_row(
    material_column(width = 4, numericInput("sigma1.11", "Covariance", value = 1, step = 0.5))
  ),
  material_row(
    material_column(width = 4, numericInput("sigma1.21", "", value = 0, step = 0.5)),
    material_column(width = 4, numericInput("sigma1.22", "", value = 1, step = 0.5))
  ),
  material_row(
    material_column(width = 4, numericInput("sigma1.31", "", value = 0, step = 0.5)),
    material_column(width = 4, numericInput("sigma1.32", "", value = 0, step = 0.5)),
    material_column(width = 4, numericInput("sigma1.33", "", value = 1, step = 0.5))
  )
) 

inMu2 <- material_row(
  material_column(width = 4, material_number_box("mean2x", "Mean x", initial_value = -1, min_value = -30, max_value = 30)),
  material_column(width = 4, material_number_box("mean2y", "y", initial_value = -1, min_value = -30, max_value = 30)),
  material_column(width = 4, material_number_box("mean2z", "z", initial_value = -1, min_value = -30, max_value = 30))
)

inSigma2 <- material_row(
  material_row(
    material_column(width = 4, numericInput("sigma2.11", "Covariance", value = 1, step = 0.5))
  ),
  material_row(
    material_column(width = 4, numericInput("sigma2.21", "", value = 0, step = 0.5)),
    material_column(width = 4, numericInput("sigma2.22", "", value = 1, step = 0.5))
  ),
  material_row(
    material_column(width = 4, numericInput("sigma2.31", "", value = 0, step = 0.5)),
    material_column(width = 4, numericInput("sigma2.32", "", value = 0, step = 0.5)),
    material_column(width = 4, numericInput("sigma2.33", "", value = 1, step = 0.5))
  )
)

inMix <- tagList(
  material_slider(
    input_id = "lambda",
    label = "Lambda - mix probability (100x)",
    initial_value = 60,
    min_value = 1, max_value = 99
  ),
  material_slider(
    input_id = "sizeN",
    label = "Data length (n)",
    initial_value = 100,
    min_value = 50, max_value = 500
  )
)

inEMextra <- material_row(
  tags$br(),
  tags$br(),
  material_column(width = 6, material_number_box(
    input_id = "maxiter",
    label = "Max Iterations",
    initial_value = 500,
    min_value = 100, max_value = 2000
  )),
  material_column(width = 6, material_number_box(
    input_id = "initMag",
    label = "InitValue Error",
    initial_value = 3,
    min_value = 1, max_value = 10
  ))
)

runEMbtn <- material_row(
  material_column(width = 6, material_button("genSim", label= "Generate Data")),
  material_column(width = 6, material_button("runEM", label = "Run EM"))
)


# modules in uni ----------------------------------------------------------

boxConfig <- material_row(
  title = "Configurations: Missingness & Imputation",
  # missingness inputs
  material_column(width = 4, material_card( 
      inMissType, 
      inMissPercent,
      p(tags$i("Data will be generated automatically"))
  )),
  # imputation inputs
  material_column(width = 4, material_card( 
      inImputeM, 
      inImputeMulti, 
      runImputation
  )),
  # graph options
  material_column(width = 4, material_card( 
      inPlotItems, 
      p(tags$i("only available after imputation | V1,...,V9,... stand for each imputation result from the multi-imputation")),
      inShowGroup
  ))
)

boxValues <- material_row(
  valueBoxOutput(width = 2, "valueBoxA"),
  valueBoxOutput(width = 2, "valueBoxB"),
  valueBoxOutput(width = 4, "valueBoxC"),
  valueBoxOutput(width = 4, "valueBoxD")
)

boxImpRes <- material_row(
  material_column(width = 5, material_card(
    title = "Result",
    dataTableOutput("missTable"),
    plotOutput("missPlot")
  )),
  material_column(width = 7, material_card(
    title = "Visualization",
    plotlyOutput("missMain")
  ))
)


# modules in EM -----------------------------------------------------------

boxEMconfig <- material_row(
  title = "Simulation Configuration",
  # distribution 1
  material_column(width = 4, material_card(title = "Distribution 1", inMu1, inSigma1)),
  # distribution 2
  material_column(width = 4, material_card(title = "Distribution 2", inMu2, inSigma2)),
  # options and run
  material_column(width = 4, material_card(title = "Mixing & EM option", inMix, inEMextra), runEMbtn)
)

boxEMRes <- material_row(
  material_column(width = 4, material_card(title = "Result", 
                                           htmlOutput("emInfo"), 
                                           verbatimTextOutput("emPrint"))),
  material_column(width = 8, material_card(title = "3D Visualization",
                                           plotlyOutput("plot3d", height = "500px")))
)


# modules in source -------------------------------------------------------

boxSource0 <- material_card(
  title = "Shiny 3-file structure",
  p("ui.R    server.R    and    global.R")
)

boxSource1 <- material_card(
  title = "ui.R" ,
  tags$pre(includeText("ui.R"))
)

boxSource2 <- material_card(
  title = "server.R",
  tags$pre(includeText("server.R"))
)

boxSource3 <- material_card(
  title = "global.R",
  tags$pre(includeText("global.R"))
)

# MAKE the UI -------------------------------------------------------------


material_page(
  title = "My Missing Data Lab",
  nav_bar_color = "cyan lighten-2",
  material_tabs(
    tabs = c("EM Algorithm" = "em", 
             "Univariate Lab" = "lab",
             "Source" = "source")
  ),
  material_tab_content(
    tab_id = "em",
    boxEMconfig,
    boxEMRes
  ),
  material_tab_content(
    tab_id = "lab",
    tags$h1("Coming Soon")
#    tags$p("I am currently working on a dynamic link to ESS (European Social Survey), and will be able to publish result here")
#     boxConfig, boxValues, boxImpRes
  ),
  material_tab_content(
    tab_id = "source",
    tags$div(
      class = "container",
      boxSource0,
      boxSource1,
      boxSource2,
      boxSource3
    )
  )
)











