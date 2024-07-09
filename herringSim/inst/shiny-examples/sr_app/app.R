################################################################################

# Spawner-Recruit App

# This Shiny application takes user-specified parameters to perform unfished
# biomass simulations of the Sitka herring stock using functional spawner-recruit
# models instead of empirical. The intent is to show users how different models
# fit to the ASA estimates and affect the simulation and threshold estimates.

# Spawner-recruit estimates from 2022 hindcast for years 1979-2021

# By: CL Roberts

################################################################################



#-------------------------------------------------------------------------------

#### Load functions, packages and data ####

#-------------------------------------------------------------------------------



library(herringSim)

library(shiny)
library(shinyWidgets)
library(magrittr)
library(stats4)
library(ggplot2)


## default starter data

data("asa2021")
sr <- sr_asa2021
mean_wt <- mean_wt_asa2021
maturity <- maturity_asa2021
S <- S_asa2021


#-------------------------------------------------------------------------------

#### Begin App ####

#-------------------------------------------------------------------------------

## UI

ui <- fluidPage(

    titlePanel(h1("Simulating with Spawner-Recruit Models")),
    br(),

    titlePanel(h3("Choose data source and years")),

    # p("This application attempts to compare the implementation of an empirical
    #   spawner-recruit model to standard functional models
    #   in unfished biomass simulations of a Sitka herring stock.
    #   The empirical model approach utilizes strata for sampling age-3 recruits
    #   from spawner biomass and is adapted from methodology in",
    #   em("Carlile (1998)."), "Spawner-recruit estimates are modeled by an
    #   age-structured assessment (ASA) hindcast for years 1979-2021."),
    # p("To select
    #   specific years from the hindcast for plotting and simulating, input a vector
    #   using R syntax in the following text box. Note that this does not change
    #   the fit of the functional spawer-recruit models, but does change the maturity
    #   and weight-at-age data. Hence, the simulations using functional spawner-recruit
    #   relationship only make sense for years 1979-2021 (for now). But simulating
    #   using the empirical relationship is valid for any combination of years for
    #   which there is data."),

    verticalLayout(

      fluidRow(

        column(3, style = "margin-left: 30px;",
               tags$div(id = "inline",
                        selectInput("data", label = h4("Select Data Source:"),
                                    choices = list("1996 ASA Hindcast" = "asa1996",
                                                   "2021 ASA Hindcast" = "asa2021",
                                                   "File Input" = "fileInput"),
                                    selected = "asa2021"))
               ),
        column(3, style = "margin-left: 30px;",
               tags$div(id = "inline",
                        textInput("years", label = h4("Years:"), value = "c(1979:2021)"))
        ),
        column(2, style = "margin-left: 10px; margin-top: 45px;", label = h4("Select Years"),
               actionButton("yearsAction", label = "Select")
        )

      ),

      fluidRow(

        conditionalPanel(condition = "input.data == 'fileInput'",
                         column(3,
                                fileInput("sr", label = h4("Spawner-Recruit"))
                         ),
                         column(3,
                                fileInput("meanWeight", label = h4("Mean Weight-at-age"))
                         ),
                         column(3,
                                fileInput("maturity", label = h4("Maturity-at-age"))
                         ),
                         column(3,
                                fileInput("S", label = h4("Survival Rate"))
                         )),
      ),
    ),

    titlePanel(h3("Fit Models to ASA Hindcast Estimates")),

    # p("The spawner-recruit relationship is plotted below. Use the check boxes to
    #   see one or more model fits to these estimates and the slider bar to see
    #   labels for specific ranges of years. If an empirical model is chosen, then
    #   select the number of strata from which age-3 recruits are sampled from."),

    br(),

    sidebarLayout(
        sidebarPanel(style = "margin-top: 20px;",
            radioButtons("modelType", label = "Model Type:",
                         choices = c("Empirical" = "empirical",
                                     "Lognormal" = "lnRecruitment",
                                     "Functional" = "fun"),
                         selected = "empirical", inline = TRUE),
            conditionalPanel(condition = "input.modelType == 'fun'",
               fluidRow(
                 column(6,
                   checkboxGroupInput("model", label = "Choose Model:",
                                      choices = c("Beverton-Holt" = "BH_mod",
                                                  "Ricker" = "R_mod",
                                                  "Shepherd" = "S_mod"),
                                      selected = c("BH_mod")
                                      )
                        ),
                 column(6, style = "margin-top: 20px;",
                   radioButtons("errorStructure",
                                label = "Error Structure",
                                choices = c("Additive" = "additive",
                                            "Multiplicative" = "multiplicative")
                                )
                        )
                       )),
            conditionalPanel(condition = "input.modelType != 'fun'",
               fluidRow(
                 column(6,
                        numericInput("strata", label = "# Strata:",
                                     value = 3, min = 1)),
                 column(6,
                        numericInput("seed", label = "Seed:",
                                     value = 123, min = 1))
               )
                             ),
            sliderInput("yearsLabel", label = "Label Years:", min = 1979,
                        max = 2021, value = c(1979, 2021), sep = "")
        ),

        mainPanel(
         plotOutput("srfig")
        )
    ),

    titlePanel(h3("Run Simulation")),

    # p("Use the fields below to choose the conditions under which the unfished
    #   biomass simulation is performed. The radio buttons allow to select which
    #   spawner-recruit model should be used and the '# years' numeric entry field
    #   specifies how many years are simulated. To try a different repeatable
    #   simulation, simply select a different seed value."),
    #
    # p("If a spawner-recruit function is chosen, then the checkbox enables you to
    #   include a lognormal error term in the simulations. The default parameter
    #   for this error term is the standard deviation of the selected model's
    #   standardized residuals. If the empirical model is chosen, then choose the
    #   number of strata from which age-3 recruits are sampled from."),
    #
    # p("Press the 'simulate' action button to perform the simulation."),

    br(),
    h4("Simulated Average Unfished Biomass of Sitka Herring"),

    wellPanel(
      verticalLayout(

        fluidRow(
          column(2, numericInput("numYears", label = "# Years:",
                                 value = 2500, min = 100, max = 10000),
                 actionButton("sim", label = "Simulate")),
          column(10, uiOutput("modelOut"))
        ),
        br(),
        plotOutput("simfig")
        # textOutput("dummy")
      )
    )

)


#-------------------------------------------------------------------------------

## Server

server <- function(input, output, session) {

  mean_wt_tmp <- reactiveValues(update = mean_wt)
  maturity_tmp <- reactiveValues(update = maturity)
  S_tmp <- reactiveValues(update = S)
  sr_tmp <- reactiveValues(update = sr)

  BH_start <- list(logalpha = 3, logbeta = -2, logsigma2 = 2)
  R_start <- list(logalpha = -10, logbeta = -10, logsigma2 = 5)
  S_start <- list(logalpha = -5, logbeta = 10, loggamma = 3, logsigma2 = 10)


  observeEvent(input$yearsAction, {

    if(input$data == "asa1996"){
      data("asa1996")
      sr <- sr_asa1996
      mean_wt <- mean_wt_asa1996
      maturity <- maturity_asa1996
      S <- S_asa1996
    } else if(input$data == "asa2021"){
      data("asa2021")
      sr <- sr_asa2021
      mean_wt <- mean_wt_asa2021
      maturity <- maturity_asa2021
      S <- S_asa2021
    }

    sr_tmp$update <- sr[sr$year %in% eval(parse(text = input$years)),]

    mean_wt_tmp$update <- mean_wt

    maturity_tmp$update <- maturity

    S_tmp$update <- S

  })

  BH_mod_list <- reactiveValues()
  R_mod_list <- reactiveValues()
  S_mod_list <- reactiveValues()

  observeEvent(input$errorStructure, {

    sr <- sr_tmp$update

    ## beverton-holt

    BH_tmp <- fit_bevertonHolt(recruits = sr$recruits, spawners = sr$spawners,
                                    start = BH_start,
                                    errorStructure = input$errorStructure)
    BH_mod_list$coef <- BH_tmp$coef
    BH_mod_list$mod <- BH_tmp$mod
    BH_mod_list$logLik <- BH_tmp$logLik

    ## ricker

    R_tmp <- fit_ricker(recruits = sr$recruits, spawners = sr$spawners,
                        start = R_start,
                        errorStructure = input$errorStructure)
    R_mod_list$coef <- R_tmp$coef
    R_mod_list$mod <- R_tmp$mod
    R_mod_list$logLik <- R_tmp$logLik

    ## shepherd

    S_tmp <- fit_shepherd(recruits = sr$recruits, spawners = sr$spawners,
                          start = S_start,
                          errorStructure = input$errorStructure)
    S_mod_list$coef <- S_tmp$coef
    S_mod_list$mod <- S_tmp$mod
    S_mod_list$logLik <- S_tmp$logLik
  })

  output$srfig <- renderPlot({

    sr <- sr_tmp$update

    sr$year <- ifelse(input$yearsLabel[1] <= sr$year & sr$year <= input$yearsLabel[2],
                      sr$year, NA)

    if(input$modelType != "fun"){

      set.seed(input$seed)
      sr$strata <- factor(kmeans(as.matrix(sr[,c("spawners", "recruits")]),
                                 centers = input$strata)$cluster)

      sr_list <- split_sr(sr$recruits, sr$spawners, sr$strata)

      if(input$strata>1){
        splits <- strata_boundaries(sr_list)
      } else {
        splits <- NULL
      }

      srfig <- plot_strata(recruits = sr$recruits, spawners = sr$spawners,
                           strata = sr$strata, splits = splits, years = sr$year)

      if(input$modelType == "lnRecruitment"){

        recruit_mids <- sapply(sr_list, FUN = function(x) mean(unlist(x["recruits"])))

        hlines <- data.frame(recruit_meds = rep(recruit_mids, lapply(sr_list, FUN = nrow)),
                             spawners_mins = rep(c(min(sr$spawners), splits),
                                                 lapply(sr_list, FUN = nrow)),
                             spawners_maxs = rep(c(splits, max(sr$spawners)),
                                                 lapply(sr_list, FUN = nrow)))

        srfig <- srfig + geom_linerange(data = hlines, aes(xmin = spawners_mins,
                                                           xmax = spawners_maxs,
                                                           y = recruit_meds))

      }

    } else if(input$modelType == "fun"){

      srfig <- plot_strata(recruits = sr$recruits, spawners = sr$spawners,
                           years = sr$year) + labs(color = "Model")

      BH_mod <<- BH_mod_list$mod
      R_mod <<- R_mod_list$mod
      S_mod <<- S_mod_list$mod

      if("BH_mod" %in% input$model){
        srfig <- srfig + geom_function(fun = BH_mod, aes(color = "Beverton-Holt"))
      }
      if("R_mod" %in% input$model){
        srfig <- srfig + geom_function(fun = R_mod, aes(color = "Ricker"))
      }
      if("S_mod" %in% input$model){
        srfig <- srfig + geom_function(fun = S_mod, aes(color = "Shepherd"))
      }

    }

    srfig

    })

    B <- eventReactive(input$sim, {

      ## use user-specified years to simulate from sr estimates, recomputing
      ## maturity, survival, and weight-at-age

      sr <- sr_tmp$update

      mean_wt <- mean_wt_tmp$update[rownames(mean_wt_tmp$update) %in% eval(parse(text = input$years)),] %>%
        colMeans()
      maturity <- maturity_tmp$update[rownames(maturity_tmp$update) %in% eval(parse(text = input$years)),] %>%
        colMeans()
      S <- S_tmp$update[rownames(S_tmp$update) %in% eval(parse(text = input$years)),] %>%
        colMeans() %>%
        unique()

      set.seed(input$seed)

      N_t <- B_sim_init(recruits = sr$recruits, S = S)

      if(input$modelType != "fun"){

        sr$strata <- factor(kmeans(as.matrix(sr[,c("spawners", "recruits")]),
                                   centers = input$strata)$cluster)

        if(input$modelType == "empirical"){

          B_sim_empirical(N_t = N_t, recruits = sr$recruits, spawners = sr$spawners,
                          strata = sr$strata, mean_wt = mean_wt, maturity = maturity,
                          S = S, num_years = input$numYears)

        } else if(input$modelType == "lnRecruitment"){

          B_sim_lnRecruitment(N_t = N_t, recruits = sr$recruits, spawners = sr$spawners,
                              strata = sr$strata,
                              mean_wt = mean_wt, maturity = maturity,
                              S = S, num_years = input$numYears)

        }

      } else if(input$modelType == "fun") {

        if(length(input$model) > 1) {
          stop("Please select only one stock-recruit model to simulate from!")
        }


        B_sim_functional(N_t = N_t, recruits = sr$recruits, spawners = sr$spawners,
                         mean_wt = mean_wt, maturity = maturity, S = S,
                         num_years = input$numYears,
                         model_type = input$model)

      }

    })

    output$modelOut <- renderUI({

      ## display stock-recruit formulas

      if(input$modelType != "fun"){

        helpText("")

      } else if(input$modelType == "fun") {

        if(length(input$model) == 0) {
          helpText("")
        } else if(length(input$model) == 1) {

          if(input$model == "BH_mod") {

            list(
              withMathJax(),
              sprintf(paste(
                "$$ R = \\frac{S}{\\alpha + \\beta S}",
                ifelse(input$errorStructure == "additive",
                       "+ \\epsilon",
                       "\\ e^{\\epsilon}"),
                "\\ \\ \\ \\ \\ \\ \\epsilon \\sim N(0, \\sigma^2) $$")),
                sprintf("$$ \\ \\ \\ \\hat{\\alpha} = %.2e
                        \\ \\ \\ \\hat{\\beta} = %.2e
                        \\ \\ \\ \\hat{\\sigma}^2 = %.2e $$",
                BH_mod_list$coef["alpha"],
                BH_mod_list$coef["beta"],
                BH_mod_list$coef["sigma2"]),
              sprintf("$$ \\text{Log-Likelihood} = %.4e \\ \\ \\ \\ \\
                                  \\text{AIC} = %.4e $$",
                      BH_mod_list$logLik,
                      -2*BH_mod_list$logLik)
            )



          } else if(input$model == "R_mod") {

            list(
              withMathJax(),
              sprintf(paste(
                "$$ R = S \\alpha e^{-\\beta S}",
                ifelse(input$errorStructure == "additive",
                       "+ \\epsilon",
                       "\\ e^{\\epsilon}"),
                "\\ \\ \\ \\ \\ \\ \\epsilon \\sim N(0, \\sigma^2) $$")),
              sprintf("$$ \\ \\ \\ \\hat{\\alpha} = %.2e
                        \\ \\ \\ \\hat{\\beta} = %.2e
                        \\ \\ \\ \\hat{\\sigma}^2 = %.2e $$",
                      R_mod_list$coef["alpha"],
                      R_mod_list$coef["beta"],
                      R_mod_list$coef["sigma2"]),
              sprintf("$$ \\text{Log-Likelihood} = %.4e \\ \\ \\ \\ \\
                                  \\text{AIC} = %.4e $$",
                      R_mod_list$logLik,
                      -2*R_mod_list$logLik)
            )


          } else if(input$model == "S_mod") {

            list(
              withMathJax(),
              sprintf(paste(
                "$$ R = \\frac{S \\alpha}{1 +
                                 \\left(\\frac{S}{\\beta}\\right)^\\gamma}",
                ifelse(input$errorStructure == "additive",
                       "+ \\epsilon",
                       "\\ e^{\\epsilon}"),
                "\\ \\ \\ \\ \\ \\ \\epsilon \\sim N(0, \\sigma^2) $$")),
              sprintf("$$ \\ \\ \\ \\hat{\\alpha} = %.2e
                        \\ \\ \\ \\hat{\\beta} = %.2e
                        \\ \\ \\ \\hat{\\gamma} = %.2e
                        \\ \\ \\ \\hat{\\sigma}^2 = %.2e $$",
                      S_mod_list$coef["alpha"],
                      S_mod_list$coef["beta"],
                      S_mod_list$coef["gamma"],
                      S_mod_list$coef["sigma2"]),
              sprintf("$$ \\text{Log-Likelihood} = %.4e \\ \\ \\ \\ \\
                                  \\text{AIC} = %.4e $$",
                      S_mod_list$logLik,
                      -2*S_mod_list$logLik)
            )

          }

        } else if(length(input$model) > 1) {
          withMathJax(sprintf("$$ \\text{Please select only one
                              stock-recruit model to simulate from!} $$"))
        }

      }

    })


    output$simfig <- renderPlot({

      ## plot simulated biomass

      plot_aub_sim(B()) + labs(title = "")

    })

    # output$dummy <- renderPrint({
    # })

}


#-------------------------------------------------------------------------------


# Run the application

shinyApp(ui = ui, server = server)
