# ui.R
ui <- dashboardPage(

  dashboardHeader(title = tagList('Mixer beta',
                                  img(src="images/logoR39.png",
                                      height = 50, align = "right"))

                  ),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    withMathJax(),
    bsModal(id = 'gif', title = 'Generating experts and mixing...',
            trigger = 'valide_expert', size = 'large',
                img(src="images/busy.gif")
            ),
    tabItems(
      # --------------------- settings tab -----------------------------
      tabItem("settings",
              # -------------- General settings - import data & model --------------
              fluidRow(
                column(6,
                       tabBox(width = 12,
                              tabPanel("Train data",
                                       fileInput("train_data",
                                                 label = h5("Train data :"))
                              ),
                              tabPanel("Test data",
                                       fileInput("test_data",
                                                 label = h5("Test data :"))
                              ),
                              tabPanel("Orig expert",
                                       textInput("orig_expert", label = h5("Original expert :")),
                                       actionButton("generate_orig", label = h5("Generate"))
                              )
                       ),
                       fluidRow(
                         valueBoxOutput('train_data_box'),
                         valueBoxOutput('test_data_box'),
                         valueBoxOutput('model_data_box')
                       )

                ),
                box(width = 6, solidHeader = TRUE, status = "primary",
                    title = "Machine learing experts",
                    checkboxGroupInput("expert_ml_choice",
                                       label = h5("Choose the machine learning experts to mix :"),
                                       choices = list("SVM polynomial" = 1,
                                                      "SVM radial" = 2,
                                                      "Xgboost" = 3,
                                                      "Random Forest" = 4,
                                                      "Projection pursuit regression" = 5,
                                                      "Keras" = 6),
                                       selected = 1:5)
                )
              ),
              # --------------------- expert specification -----------------------------
              fluidRow(
                box(width = 6,
                    solidHeader = TRUE,
                    id = 'expert_choice_box', status = "primary",
                    title = 'Strategies to mix ',
                    checkboxGroupInput("expert_choice",
                                       label = h5("Choose the strategies to mix :"),
                                       choices = list("Constant bias" = 1,
                                                      "Bagging" = 2,
                                                      "Boosting" = 3,
                                                      "Random walk" = 4,
                                                      "QGam" = 5,
                                                      "Gamlss" = 6),
                                       selected = 1:6)
                ),
                uiOutput('tabs')
              ),
              # --------------------- recap expert param -----------------------------
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    title = "Strategy Parameter",
                    textOutput('recap_expert_param'),
                    tags$style(type="text/css",
                               "#recap_expert_param {white-space: pre-wrap;}"),
                    br(),
                    actionButton('valide_expert', 'Generate Experts')
                )
              )
      ),
      # --------------------- expert tab -----------------------------
      tabItem("expert",
              box(title = 'Analysis by group of experts', width = 12,
                  uiOutput('plot_by_group')
              ),
              box(title = 'Prediction by group of experts', width = 12,
                  uiOutput('plot_prediction_by_group')
              ),
              box(title = 'Residual by group of experts', width = 12,
                  uiOutput('plot_res_by_group')
              )
      ),
      # --------------------- oracle tab -----------------------------
      tabItem("oracle",
              box('Metrics', width = 12,
                  plotlyOutput('plot_metrics')),
              box('Compare', width = 12,
                  plotlyOutput('plot_compare_strategy'))
      ),
      # --------------------- mix tab -----------------------------
      tabItem("mix",
              box('RMSE - Root of mean square error', width = 6,
                  helpText('$$RMSE = \\sqrt{\\sum_{i=1}^{n} \\frac{(x_i - \\hat{x_i})^2}{n}}$$')
              ),
              box('MAPE - Mean Absolute Percentage Error', width = 6,
                  helpText('$$MAPE = \\frac{1}{n} \\sum_{i=1}^{n} |\\frac{x_i - \\hat{x_i}}{x_i}| $$')
              ),
              box(title = "Table of evaluation metrics", solidHeader = TRUE,
                  background = "blue", width = 12,
                  tableOutput('res'))
      )
    )
  )
)
