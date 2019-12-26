# server.R
server <- function(input, output, session) {
  server_data <- reactiveValues(orig_expert = NULL)

  # ------------------ tab expert ------------------
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Settings", tabName = "settings", icon = icon("cube")),
      menuItem("Experts", tabName = "expert", icon = icon("cube")),
      menuItem("Oracle", tabName = 'oracle', icon = icon("cubes")),
      menuItem("Mix", tabName = "mix", icon = icon("cubes"))
    )
  })

  output$recap_expert_param <- renderText({
    all_text <- list(
      paste0('Constant bias : \n',
             ' - Number of expert : ', input$n_expert_cb, '\n',
             ' - Amplitude : ', input$amplitude_cb, '\n'),
      paste0('Bagging : \n',
             ' - Number of expert : ', input$n_expert_bagg, '\n',
             ' - Sampling rate : ', input$sr_bagg, '\n'),
      paste0('Boosting : \n',
             ' - Number of expert : ', input$n_expert_boost, '\n',
             ' - Lambda : ', input$lambda_boost, '\n'),
      paste0('Random Walk : \n',
             ' - Number of expert : ', input$n_expert_rw, '\n',
             ' - Variance factor : ', input$var_fact_rw, '\n'),
      paste0('QGam : \n',
             ' - Number of expert : ', input$n_expert_qgam, '\n'),
      paste0('Gamlss : \n',
             ' - Number of expert : ', input$n_expert_gamlss, '\n')
    )

    if (!is.null(input$expert_choice)) {
      text_recap <- lapply(as.numeric(input$expert_choice), function(i) {
        if (i <= 4)
        all_text[[i]]
      })
      do.call(paste0, text_recap)
    }
  })

  observeEvent(input$valide_expert, {
    server_data$list_expert <- list()
    if (!is.null(input$expert_ml_choice)) {
      server_data$list_expert$ml_expert <- list()
    }
    # -------------- Constant bias ---------------
    if (is.null(input$pretrained_cb)) {
      cverr <- sqrt(server_data$orig_expert$gcv.ubre)
      grid.bias <- seq(-input$amplitude_cb, input$amplitude_cb,
                      length = input$n_expert_cb) * cverr
      cb <- lapply(grid.bias, function(x){
        x + server_data$orig_expert$forecast
        }) %>%
        unlist %>%
        matrix(., ncol = input$n_expert_cb,
               nrow = nrow(server_data$test_data))

      colnames(cb) <- paste0("cb_", 1:input$n_expert_cb)
      server_data$list_expert$cb <- cb
    } else {
      cb <- readRDS(input$pretrained_cb$datapath)
      server_data$list_expert$cb <- cb
    }
    # -------------- Random walk ---------------
    if (is.null(input$pretrained_rw)) {
      Narima <- input$n_expert_rw
      cverr <- sqrt(server_data$orig_expert$gcv.ubre)
      rw <- matrix(0, nrow = nrow(server_data$test_data),
                         ncol = Narima)
      for(i in c(1:Narima)) {
        randomwalk <- cumsum(rnorm(n = nrow(server_data$test_data),
                                   mean = 0,
                                   sd = cverr / input$var_fact_rw))
        rw[, i] <- server_data$orig_expert$forecast + randomwalk
      }
      colnames(rw) <- paste0("rw_", 1:Narima)
      server_data$list_expert$rw <- rw
    } else {
      rw <- readRDS(input$pretrained_rw$datapath)
      server_data$list_expert$rw <- rw
    }
    # -------------- Bagging ---------------
    if (is.null(input$pretrained_bagg)) {
      Nbag<- input$n_expert_cb
      n <- nrow(server_data$train_data)
      gam.bagg <- bagging(input$n_expert_cb,
                          data_app = server_data$train_data,
                          server_data$orig_expert$formula,
                          size = floor(input$sr_bagg * n), seed = 1)

      bagg.forecast <- lapply(gam.bagg, predict,
                              newdata = server_data$test_data)
      bagg <- matrix(c(server_data$orig_expert$forecasst,
                       unlist(bagg.forecast)),
                     ncol = Nbag,
                     nrow = nrow(server_data$test_data),
                     byrow = FALSE)
      colnames(bagg) <- paste0('bagg_', 1:Nbag)
      server_data$list_expert$bagg <- bagg
    } else {
      bagg <- readRDS(input$pretrained_bagg$datapath)
      server_data$list_expert$bagg <- bagg
    }
    # -------------- Boosting ---------------
    if (is.null(input$pretrained_boost)) {
      gamma <- input$lambda_boost
      Nboost <- input$n_expert_boost
      gamboost <- list()
      gamboost[[1]] <- server_data$orig_expert
      yres <- server_data$train_data$Load
      equation <- as.formula(paste0("yres ~ ", as.character(server_data$orig_expert$formula)[3]))
      for(i in c(2:Nboost)){
        yres <- (yres - gamma * gamboost[[i-1]]$fitted.values) / (1 - gamma)
        gamboost[[i]] <- gam(equation, data = server_data$train_data)
        gamboost[[i]]$forecast <- predict(gamboost[[i]],
                                          server_data$test_data)
      }
      boost <- lapply(gamboost, function(x){x$forecast}) %>%
        unlist() %>%
        matrix(., byrow = F, ncol = Nboost)
      colnames(boost) <- paste0("boost_", 1:Nboost)
      server_data$list_expert$boost <- boost
    } else {
      boost <- readRDS(input$pretrained_boost$datapath)
      server_data$list_expert$boost <- boost
    }
    # -------------- QGam ---------------
    if (is.null(input$pretrained_qgam)) {
      quList <- c(0.05, 0.25, 0.5, 0.75, 0.95)
      qgammodel <- list()
      for(j in c(1:length(quList))){
        qgammodel[[j]] <- qgam(server_data$orig_expert$formula,
                               data = server_data$train_data, err = 0.05,
                               qu = quList[j], control = list("tol" = 0.01))
      }
      qgam <- lapply(qgammodel, predict,
                     newdata = server_data$test_data) %>%
        unlist %>%
        matrix(., byrow = F, ncol = length(quList))
      colnames(qgam) <- paste0("qgam_", quList)
      server_data$list_expert$qgam <- qgam
    } else {
      qgam <- readRDS(input$pretrained_qgam$datapath)
      server_data$list_expert$qgam <- qgam
    }
    # -------------- Gamlss ---------------
    if (is.null(input$pretrained_gamlss)) {
      quList <- c(0.05, 0.25, 0.5, 0.75, 0.95)
      equation <- Load~s(Time,k=4)+s(NumWeek,k=30, bs='cc')+te(Time, Temp, k=c(3, 5))+s(Load1, k=10)+IPI_CVS+s(Temp1)
      gamLss <- gam(list(equation, ~ s(Temp)),
                    data = server_data$train_data, family = gaulss)
      gamlss.forecast <- predict(gamLss, newdata = server_data$test_data,
                                 type = 'response')
      sd.gamlsss.forecast <- 1 / gamlss.forecast[, 2]
      quantNorm.lss <- lapply(quList, normQuant.lss, sd = sd.gamlsss.forecast)
      prevNorm.lssForecast <- lapply(quantNorm.lss, function(x){
        x + gamlss.forecast[, 1]
        })

      gamlss <- prevNorm.lssForecast %>%
        unlist %>%
        matrix(., byrow = F, ncol = length(quList))
      colnames(gamlss) <- paste0("gamlss_", quList)
      server_data$list_expert$gamlss <- gamlss
    } else {
      gamlss <- readRDS(input$pretrained_gamlss$datapath)
      server_data$list_expert$gamlss <- gamlss
    }

    # ------------ xgboost ---------------
    if (3 %in% as.numeric(input$expert_ml_choice)) {
      month0 <- dummy(server_data$train_data$Month)
      month1 <- dummy(server_data$test_data$Month)
      dataxgb0 <- server_data$train_data %>%
        as.tibble() %>%
        bind_cols(data.frame(month0)) %>%
        dplyr::select(- Month) %>%
        dplyr::select(- Day) %>%
        dplyr::select(- Load)
      dataxgb1 <- server_data$test_data %>%
        as.tibble() %>%
        bind_cols(data.frame(month1)) %>%
        dplyr::select(- Month) %>%
        dplyr::select(- Day) %>%
        dplyr::select(- Load)
      dtrain <- xgb.DMatrix(data = as.matrix(server_data$train_data[, -c(4, 6)]),
                            label = log(server_data$train_data$Load))
      dtest <- xgb.DMatrix(data = as.matrix(server_data$test_data[, -c(4, 6)]),
                           label = log(server_data$test_data$Load))

      set.seed(52)
      xgbexp <- xgb.train(params = list(eta = 0.3, max.depth = 3,
                                        gamma = 0, min_child_weight = 2,
                                        subsample = 0.8, colsample_bytree = 0.8),
                          data = dtrain, watchlist = list(train = dtrain),
                          objective = "reg:linear", nround = 1000,
                          booster = "gbtree", verbose = 0)

      xgboost <- as.matrix(exp(predict(xgbexp, dtest)), ncol = 1)
      colnames(xgboost) <- 'xgboost'
      server_data$list_expert$ml_expert$xgboost <- xgboost
    }

    # ------------ svmRadial ---------------
    if (2 %in% as.numeric(input$expert_ml_choice)) {
      pre_proc_values <- preProcess(server_data$train_data,
                                    method = c("center", "scale"))
      df_train_trans <- predict(pre_proc_values, server_data$train_data)
      df_test_trans <- predict(pre_proc_values, server_data$test_data)
      test_x_trans <- df_test_trans %>%
        select(., - Load)
      test_y_trans <- df_test_trans$Load
      svm_ctrl <- trainControl(method = "cv", number = 10)
      this_grid <- expand.grid(sigma = 6 * 10^-3,
                               C = 1000)
      input_x <- df_train_trans %>%
        select(- Load)
      input_y <- df_train_trans %>%
        .[['Load']]
      svm_caret <- caret::train(input_x, input_y,
                                method = 'svmRadial',
                                tuneGrid = this_grid,
                                preProc = NULL,
                                metric = 'RMSE',
                                maximize = FALSE,
                                trControl = svm_ctrl)
      pred_svmRadial <- predict(svm_caret, test_x_trans)
      svmRadial <- as.matrix(retransform_load(pred_svmRadial, pre_proc_values))
      colnames(svmRadial) <- 'svmRadial'
      # svmRadial <- readRDS('../../extdata/expert_svm.rds')
      server_data$list_expert$ml_expert$svmRadial <- svmRadial
    }

    # ------------ svmPoly ---------------
    if (1 %in% as.numeric(input$expert_ml_choice)) {
      this_grid <- expand.grid(degree = 2,
                               scale = 0.005,
                               C = 120)
      svm_caret <- caret::train(input_x, input_y,
                                method = 'svmPoly',
                                tuneGrid = this_grid,
                                preProc = NULL,
                                metric = 'RMSE',
                                maximize = FALSE,
                                trControl = svm_ctrl)
      pred_svm_caret <- predict(svm_caret, test_x_trans)
      svmPoly <- as.matrix(retransform_load(pred_svm_caret, pre_proc_values))
      colnames(svmPoly) <- 'svmPoly'
      # svmPoly <- readRDS('../../extdata/expert_svmPoly.rds')
      server_data$list_expert$ml_expert$svmPoly <- svmPoly
    }

    # ------------ ppr ---------------
    if (5 %in% as.numeric(input$expert_ml_choice)) {
      input_x <- server_data$train_data %>%
        select(-Load)
      input_y <- server_data$train_data %>%
        select(Load)
      test_x <- server_data$test_data %>%
        select(-Load)
      ppr <- ppr(x = input_x, y = input_y,
                 nterms = 3, max.terms = 4,
                 sm.method = 'spline')
      ppr <- as.matrix(predict(ppr, test_x), ncol = 1)
      colnames(ppr) <- 'ppr'
      # ppr <- readRDS('../../extdata/expert_ppr.rds')
      server_data$list_expert$ml_expert$ppr <- ppr
    }

    # ------------ ranger ---------------
    if (4 %in% as.numeric(input$expert_ml_choice)) {
      rfexp <- ranger::ranger(Load ~ ., mtry = 6,
                           num.trees = 1500,
                           min.node.size = 4,
                           seed = 19,
                           data = server_data$train_data,
                           quantreg = TRUE)
      pred_rf <- predict(rfexp, server_data$test_data)
      preds <- predict(rfexp, server_data$test_data,
                       quantiles = seq(0.05, 0.95, 0.1),
                       type = 'quantiles')
      rf <- data.matrix(cbind(pred_rf$predictions,
                                   preds$predictions))
      colnames(rf) <- c('randomForest',
                             paste0('rf_', seq(0.05, 0.95, 0.1)))
      server_data$list_expert$ml_expert$rf <- rf
    }
    server_data$list_expert$ml_expert <- do.call(cbind,
                                                 server_data$list_expert$ml_expert)
    toggleModal(session, 'gif', toggle = "close")
  })

  output$train_data_box <- renderValueBox({
    create_status_box(input$train_data, 'Train data set')
  })

  output$test_data_box <- renderValueBox({
    create_status_box(input$test_data, 'Test data set')
  })

  observeEvent(input$generate_orig, {
    equation <- as.formula(input$orig_expert)
    server_data$train_data <- read.table(input$train_data$datapath,
                                         header = TRUE)[-1, ]
    server_data$test_data <- read.table(input$test_data$datapath,
                                         header = TRUE)[-1, ]
    server_data$orig_expert <- mgcv::gam(equation,
                                   data = server_data$train_data)
    server_data$orig_expert$forecast <- predict(server_data$orig_expert,
                                                server_data$test_data)
    server_data$orig_expert$rmse <- rmse(server_data$test_data$Load,
                                         server_data$orig_expert$forecast)
  })

  output$model_data_box <- renderValueBox({
    create_status_box(server_data$orig_expert, 'Original expert')
  })

  # ------------------ tab oracle ------------------
  # list_strategy <- reactive(server_data$list_strategy)
  output$plot_metrics <- renderPlotly(
    ggplotly(plot_metrics_expert(server_data$test_data,
                                 server_data$list_expert,
                                 server_data$orig_expert$rmse))
  )

  output$plot_compare_strategy <- renderPlotly(
    ggplotly(plot_compare(server_data$test_data,
                          server_data$list_expert,
                          server_data$orig_expert$rmse))
  )

  # ------ Params tab ---------
  output$tabs <- renderUI({
    if (!is.null(input$expert_choice)) {
      tabs <- lapply(as.numeric(input$expert_choice), function(i) {
        tabPanel(tabs.content[[i]]$Title, tabs.content[[i]]$Content)
      })
      do.call(tabBox, tabs)
    }
  })


  # ------ Plot mix tab ---------
  output$plot_by_group <- renderUI({
    if (!is.null(input$expert_choice)) {
      tabs <- lapply(1:length(tabs.plot), function(i) {
        tabPanel(tabs.plot[[i]]$Title, tabs.plot[[i]]$Content)
      })
      do.call(tabBox, c(tabs, width = 12, height = 600))
    }
  })

  tabs.plot <- list(
    # ------ Constant bias params ---------
    list(Title = "Constant_bias", Content = tagList(
      plotOutput('cb_mix', width = "100%")
    )),
    # ------ Bagging params ---------
    list(Title = "Bagging", Content = tagList(
      plotOutput('bagg_mix', width = "100%")
    )),
    # ------ Boosting params ---------
    list(Title = "Boosting", Content = tagList(
      plotOutput('boost_mix', width = "100%")
    )),
    # ------ Rw params ---------
    list(Title = "Random walk", Content = tagList(
      plotOutput('rw_mix', width = "100%")
    )),
    # ------ Qgam params ---------
    list(Title = "QGam", Content = tagList(
      plotOutput('qgam_mix', width = "100%")
    )),
    # ------ Gamlss params ---------
    list(Title = "Gamlss", Content = tagList(
      plotOutput('gamlss_mix', width = "100%")
    )),
    # ------ ML ---------
    list(Title = 'ML', Content = tagList(
      plotOutput('ml_mix', width = "100%")
    )),
    # ------ ML no gam---------
    list(Title = 'ML_no_orig', Content = tagList(
      plotOutput('ml_mix_no_gam', width = "100%")
    ))
    )

  output$cb_mix <- renderPlot({
    plot_mix(server_data$test_data$Load, server_data$orig_expert$forecast,
             server_data$list_expert$cb)
  }, width = 'auto')
  output$rw_mix <- renderPlot({
    plot_mix(server_data$test_data$Load, server_data$orig_expert$forecast,
             server_data$list_expert$rw)
  }, width = 'auto')
  output$bagg_mix <- renderPlot({
    plot_mix(server_data$test_data$Load, server_data$orig_expert$forecast,
             server_data$list_expert$bagg)
  }, width = 'auto')
  output$boost_mix <- renderPlot({
    plot_mix(server_data$test_data$Load, server_data$orig_expert$forecast,
             server_data$list_expert$boost)
  }, width = 'auto')
  output$qgam_mix <- renderPlot({
    plot_mix(server_data$test_data$Load, server_data$orig_expert$forecast,
             server_data$list_expert$qgam)
  }, width = 'auto')
  output$gamlss_mix <- renderPlot({
    plot_mix(server_data$test_data$Load, server_data$orig_expert$forecast,
             server_data$list_expert$gamlss)
  }, width = 'auto')

  output$ml_mix <- renderPlot({
    # ml_exp <- as.matrix(do.call(cbind,
    #                             server_data$list_expert$ml_expert))
    plot_mix(server_data$test_data$Load, server_data$orig_expert$forecast,
             server_data$list_expert$ml_expert)
  }, width = 'auto')

  output$ml_mix_no_gam <- renderPlot({
    # ml_exp <- as.matrix(do.call(cbind,
    #                             server_data$list_expert$ml_expert))
    plot_mix_no_gam(server_data$test_data$Load,
                    server_data$list_expert$ml_expert)
  }, width = 'auto')



  # ------ Plot prediction tab ---------
  output$plot_prediction_by_group <- renderUI({
    if (!is.null(input$expert_choice)) {
      tabs_pred <- lapply(1:length(tabs.plot_pred), function(i) {
        tabPanel(tabs.plot_pred[[i]]$Title, tabs.plot_pred[[i]]$Content)
      })
      do.call(tabBox, c(tabs_pred, width = 12, height = 600))
    }
  })

  tabs.plot_pred <- list(
    # ------ Constant bias params ---------
    list(Title = "Constant_bias", Content = tagList(
      plotlyOutput('cb_pred_mix', width = "100%")
    )),
    # ------ Bagging params ---------
    list(Title = "Bagging", Content = tagList(
      plotlyOutput('bagg_pred_mix', width = "100%")
    )),
    # ------ Boosting params ---------
    list(Title = "Boosting", Content = tagList(
      plotlyOutput('boost_pred_mix', width = "100%")
    )),
    # ------ Rw params ---------
    list(Title = "Random walk", Content = tagList(
      plotlyOutput('rw_pred_mix', width = "100%")
    )),
    # ------ Qgam params ---------
    list(Title = "QGam", Content = tagList(
      plotlyOutput('qgam_pred_mix', width = "100%")
    )),
    # ------ Gamlss params ---------
    list(Title = "Gamlss", Content = tagList(
      plotlyOutput('gamlss_pred_mix', width = "100%")
    )),
    # ------ ML ---------
    list(Title = 'ML', Content = tagList(
      plotlyOutput('ml_pred_mix', width = "100%")
    )),
    # ------ ML no gam---------
    list(Title = 'ML_no_orig', Content = tagList(
      plotlyOutput('ml_pred_mix_no_gam', width = "100%")
    ))
  )

  output$cb_pred_mix <- renderPlotly({
    plot_prediction(server_data$test_data, server_data$orig_expert$forecast,
             server_data$list_expert$cb)
  })
  output$rw_pred_mix <- renderPlotly({
    plot_prediction(server_data$test_data, server_data$orig_expert$forecast,
             server_data$list_expert$rw)
  })
  output$bagg_pred_mix <- renderPlotly({
    plot_prediction(server_data$test_data, server_data$orig_expert$forecast,
             server_data$list_expert$bagg)
  })
  output$boost_pred_mix <- renderPlotly({
    plot_prediction(server_data$test_data, server_data$orig_expert$forecast,
             server_data$list_expert$boost)
  })
  output$qgam_pred_mix <- renderPlotly({
    plot_prediction(server_data$test_data, server_data$orig_expert$forecast,
             server_data$list_expert$qgam)
  })
  output$gamlss_pred_mix <- renderPlotly({
    plot_prediction(server_data$test_data, server_data$orig_expert$forecast,
             server_data$list_expert$gamlss)
  })

  output$ml_pred_mix <- renderPlotly({
    # ml_exp <- as.matrix(do.call(cbind,
    #                             server_data$list_expert$ml_expert))
    plot_prediction(server_data$test_data, server_data$orig_expert$forecast,
                    server_data$list_expert$ml_expert)
  })

  output$ml_pred_mix_no_gam <- renderPlotly({
    # ml_exp <- as.matrix(do.call(cbind,
    #                             server_data$list_expert$ml_expert))
    plot_prediction(data_test = server_data$test_data,
                    experts = server_data$list_expert$ml_expert)
  })


  # ------ Plot bias tab ---------
  output$plot_res_by_group <- renderUI({
    if (!is.null(input$expert_choice)) {
      tabs_res <- lapply(1:length(tabs.plot_res), function(i) {
        tabPanel(tabs.plot_res[[i]]$Title, tabs.plot_res[[i]]$Content)
      })
      do.call(tabBox, c(tabs_res, width = 12, height = 600))
    }
  })

  tabs.plot_res <- list(
    # ------ Constant bias params ---------
    list(Title = "Constant_bias", Content = tagList(
      plotlyOutput('cb_res_mix', width = "100%")
    )),
    # ------ Bagging params ---------
    list(Title = "Bagging", Content = tagList(
      plotlyOutput('bagg_res_mix', width = "100%")
    )),
    # ------ Boosting params ---------
    list(Title = "Boosting", Content = tagList(
      plotlyOutput('boost_res_mix', width = "100%")
    )),
    # ------ Rw params ---------
    list(Title = "Random walk", Content = tagList(
      plotlyOutput('rw_res_mix', width = "100%")
    )),
    # ------ Qgam params ---------
    list(Title = "QGam", Content = tagList(
      plotlyOutput('qgam_res_mix', width = "100%")
    )),
    # ------ Gamlss params ---------
    list(Title = "Gamlss", Content = tagList(
      plotlyOutput('gamlss_res_mix', width = "100%")
    )),
    # ------ ML ---------
    list(Title = 'ML', Content = tagList(
      plotlyOutput('ml_res_mix', width = "100%")
    )),
    # ------ ML no gam---------
    list(Title = 'ML_no_orig', Content = tagList(
      plotlyOutput('ml_res_mix_no_gam', width = "100%")
    ))
  )

  output$cb_res_mix <- renderPlotly({
    plot_residual(server_data$test_data, server_data$orig_expert$forecast,
                    server_data$list_expert$cb)
  })
  output$rw_res_mix <- renderPlotly({
    plot_residual(server_data$test_data, server_data$orig_expert$forecast,
                    server_data$list_expert$rw)
  })
  output$bagg_res_mix <- renderPlotly({
    plot_residual(server_data$test_data, server_data$orig_expert$forecast,
                    server_data$list_expert$bagg)
  })
  output$boost_res_mix <- renderPlotly({
    plot_residual(server_data$test_data, server_data$orig_expert$forecast,
                    server_data$list_expert$boost)
  })
  output$qgam_res_mix <- renderPlotly({
    plot_residual(server_data$test_data, server_data$orig_expert$forecast,
                    server_data$list_expert$qgam)
  })
  output$gamlss_res_mix <- renderPlotly({
    plot_residual(server_data$test_data, server_data$orig_expert$forecast,
                    server_data$list_expert$gamlss)
  })

  output$ml_res_mix <- renderPlotly({
    # ml_exp <- as.matrix(do.call(cbind,
    #                             server_data$list_expert$ml_expert))
    plot_residual(server_data$test_data, server_data$orig_expert$forecast,
                  server_data$list_expert$ml_expert)
  })

  output$ml_res_mix_no_gam <- renderPlotly({
    # ml_exp <- as.matrix(do.call(cbind,
    #                             server_data$list_expert$ml_expert))
    plot_residual(data_test = server_data$test_data,
                    experts = server_data$list_expert$ml_expert)
  })


  output$plot_coef_final <- renderPlotly({
    experts <- do.call(cbind, server_data$list_expert)
    experts <- cbind(experts, server_data$orig_expert$forecast)
    colnames(experts)[ncol(experts)] <- 'orig'

    or <- oracle(Y = server_data$test_data$Load,
                 experts = experts,
                 model = "convex", loss.type = "square")
    to_plot <- data.frame(id = seq(1:ncol(experts)),
                          coef = sort(or$coefficients, decreasing = T))
    ggplotly(ggplot(to_plot) +
      geom_point(aes(x = id, y = coef)))
  })

  output$res <- renderTable({
    experts <- do.call(cbind, server_data$list_expert)
    experts <- cbind(experts, server_data$orig_expert$forecast)
    colnames(experts)[ncol(experts)] <- 'gam'
    rmse <- apply(experts, 2, function(x) {
      rmse(server_data$test_data$Load, x)
    })
    mape <- apply(experts, 2, function(x) {
      round(mape(server_data$test_data$Load, x) * 100, 2)
    })
    res <- t(rbind(rmse, mape))
    selected <- c('gam', 'xgboost', 'svmRadial', 'svmPoly', 'ppr')
    res <- res[rownames(res) %in% selected, ]
    # ml mix
    ml_exp <- server_data$list_expert$ml_expert
    mix_ml_exp <- mixture(Y = server_data$test_data$Load,
                          experts = ml_exp,
                          model = 'MLpol',
                          loss.gradient = T)
    res <- rbind(res, data.frame(rmse = rmse(server_data$test_data$Load,
                                     mix_ml_exp$prediction),
                mape = round(100 * mape(server_data$test_data$Load,
                            mix_ml_exp$prediction)),
                row.names = 'ml_mix'))

    # all mix
    all_exp <- do.call(cbind, server_data$list_expert)
    mix_all_exp <- mixture(Y = server_data$test_data$Load,
                          experts = all_exp,
                          model = 'MLpol',
                          loss.gradient = T)
    res <- rbind(res, data.frame(rmse = rmse(server_data$test_data$Load,
                                             mix_all_exp$prediction),
                                 mape = round(100 * mape(server_data$test_data$Load,
                                             mix_all_exp$prediction)),
                                 row.names = 'strategy_mix'))

    # strategy mix
    exp <- all_exp[, - c(1:4)]
    mix_exp <- mixture(Y = server_data$test_data$Load,
                       experts = exp,
                       model = 'MLpol',
                       loss.gradient = T)
    res <- rbind(res, data.frame(rmse = rmse(server_data$test_data$Load,
                                             mix_exp$prediction),
                                 mape = round(100 * mape(server_data$test_data$Load,
                                                         mix_exp$prediction)),
                                 row.names = 'all_mix'))
  }, rownames = TRUE)
  # ------------------ end session ------------------
  session$onSessionEnded(function() {
    stopApp()
  })
}
