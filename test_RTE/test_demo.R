# # test fonctionnel
# server_data <- list()
#
#
# equation <- Load ~ s(Time,k=3)+s(NumWeek,k=30, bs='cc') +
#   te(Time, Temp, k=c(3, 5))+s(Load1, k=10) + IPI_CVS + s(Temp1)
#
# server_data$train_data <- read.table("./inst/extdata/data_conso_hebdo0.txt",
#                                      header=TRUE)[-1, ]
# server_data$test_data <- read.table("./inst/extdata/data_conso_hebdo1.txt",
#                                     header=TRUE)[-1, ]
# server_data$orig_expert <- mgcv::gam(equation,
#                                      data = server_data$train_data)
# server_data$orig_expert$forecast <- predict(server_data$orig_expert,
#                                             server_data$test_data)
# server_data$orig_expert$rmse <- rmse(server_data$test_data$Load,
#                                      server_data$orig_expert$forecast)
# server_data$list_expert <- list()
# server_data$list_expert$ml_expert <- list()
#
# cverr <- sqrt(server_data$orig_expert$gcv.ubre)
# grid.bias <- seq(-2, 2,
#                  length = 10) * cverr
# cb <- lapply(grid.bias, function(x){
#   x + server_data$orig_expert$forecast
# }) %>%
#   unlist %>%
#   matrix(., ncol = 10,
#          nrow = nrow(server_data$test_data))
#
# colnames(cb) <- paste0("cb_", 1:10)
# server_data$list_expert$cb <- cb
#
# Narima <- 10
# cverr <- sqrt(server_data$orig_expert$gcv.ubre)
# rw <- matrix(0, nrow = nrow(server_data$test_data),
#              ncol = Narima)
# for(i in c(1:Narima)) {
#   randomwalk <- cumsum(rnorm(n = nrow(server_data$test_data),
#                              mean = 0,
#                              sd = cverr / 4))
#   rw[, i] <- server_data$orig_expert$forecast + randomwalk
# }
# colnames(rw) <- paste0("rw_", 1:Narima)
# server_data$list_expert$rw <- rw
#
#
# Nbag<- 10
# n <- nrow(server_data$train_data)
# gam.bagg <- bagging(10,
#                     data_app = server_data$train_data,
#                     server_data$orig_expert$formula,
#                     size = floor(0.8 * n), seed = 1)
#
# bagg.forecast <- lapply(gam.bagg, predict,
#                         newdata = server_data$test_data)
# bagg <- matrix(c(server_data$orig_expert$forecasst,
#                  unlist(bagg.forecast)),
#                ncol = Nbag,
#                nrow = nrow(server_data$test_data),
#                byrow = FALSE)
# colnames(bagg) <- paste0('bagg_', 1:Nbag)
# server_data$list_expert$bagg <- bagg
#
#
# gamma <- 0.1
# Nboost <- 10
# gamboost <- list()
# gamboost[[1]] <- server_data$orig_expert
# yres <- server_data$train_data$Load
# equation <- as.formula(paste0("yres ~ ", as.character(server_data$orig_expert$formula)[3]))
# for(i in c(2:Nboost)){
#   yres <- (yres - gamma * gamboost[[i-1]]$fitted.values) / (1 - gamma)
#   gamboost[[i]] <- gam(equation, data = server_data$train_data)
#   gamboost[[i]]$forecast <- predict(gamboost[[i]],
#                                     server_data$test_data)
# }
# boost <- lapply(gamboost, function(x){x$forecast}) %>%
#   unlist() %>%
#   matrix(., byrow = F, ncol = Nboost)
# colnames(boost) <- paste0("boost_", 1:Nboost)
# server_data$list_expert$boost <- boost
#
#
# quList <- c(0.05, 0.25, 0.5, 0.75, 0.95)
# qgammodel <- list()
# for(j in c(1:length(quList))){
#   qgammodel[[j]] <- qgam(server_data$orig_expert$formula,
#                          data = server_data$train_data, err = 0.05,
#                          qu = quList[j], control = list("tol" = 0.01))
# }
# qgam <- lapply(qgammodel, predict,
#                newdata = server_data$test_data) %>%
#   unlist %>%
#   matrix(., byrow = F, ncol = length(quList))
# colnames(qgam) <- paste0("qgam_", quList)
# server_data$list_expert$qgam <- qgam
#
#
# quList <- c(0.05, 0.25, 0.5, 0.75, 0.95)
# equation <- Load~s(Time,k=4)+s(NumWeek,k=30, bs='cc')+te(Time, Temp, k=c(3, 5))+s(Load1, k=10)+IPI_CVS+s(Temp1)
# gamLss <- gam(list(equation, ~ s(Temp)),
#               data = server_data$train_data, family = gaulss)
# gamlss.forecast <- predict(gamLss, newdata = server_data$test_data,
#                            type = 'response')
# sd.gamlsss.forecast <- 1 / gamlss.forecast[, 2]
# quantNorm.lss <- lapply(quList, normQuant.lss, sd = sd.gamlsss.forecast)
# prevNorm.lssForecast <- lapply(quantNorm.lss, function(x){
#   x + gamlss.forecast[, 1]
# })
#
# gamlss <- prevNorm.lssForecast %>%
#   unlist %>%
#   matrix(., byrow = F, ncol = length(quList))
# colnames(gamlss) <- paste0("gamlss_", quList)
# server_data$list_expert$gamlss <- gamlss
#
#
# month0 <- dummy(server_data$train_data$Month)
# month1 <- dummy(server_data$test_data$Month)
# dataxgb0 <- server_data$train_data %>%
#   as.tibble() %>%
#   bind_cols(data.frame(month0)) %>%
#   dplyr::select(- Month) %>%
#   dplyr::select(- Day) %>%
#   dplyr::select(- Load)
# dataxgb1 <- server_data$test_data %>%
#   as.tibble() %>%
#   bind_cols(data.frame(month1)) %>%
#   dplyr::select(- Month) %>%
#   dplyr::select(- Day) %>%
#   dplyr::select(- Load)
# dtrain <- xgb.DMatrix(data = as.matrix(server_data$train_data[, -c(4, 6)]),
#                       label = log(server_data$train_data$Load))
# dtest <- xgb.DMatrix(data = as.matrix(server_data$test_data[, -c(4, 6)]),
#                      label = log(server_data$test_data$Load))
#
# set.seed(52)
# xgbexp <- xgb.train(params = list(eta = 0.3, max.depth = 3,
#                                   gamma = 0, min_child_weight = 2,
#                                   subsample = 0.8, colsample_bytree = 0.8),
#                     data = dtrain, watchlist = list(train = dtrain),
#                     objective = "reg:linear", nround = 1000,
#                     booster = "gbtree", verbose = 0)
#
# xgboost <- as.matrix(exp(predict(xgbexp, dtest)), ncol = 1)
# colnames(xgboost) <- 'xgboost'
# server_data$list_expert$ml_expert$xgboost <- xgboost
#
#
# pre_proc_values <- preProcess(server_data$train_data,
#                               method = c("center", "scale"))
# df_train_trans <- predict(pre_proc_values, server_data$train_data)
# df_test_trans <- predict(pre_proc_values, server_data$test_data)
# test_x_trans <- df_test_trans %>%
#   select(., - Load)
# test_y_trans <- df_test_trans$Load
# svm_ctrl <- trainControl(method = "cv", number = 10)
# this_grid <- expand.grid(sigma = 6 * 10^-3,
#                          C = 1000)
# input_x <- df_train_trans %>%
#   select(- Load)
# input_y <- df_train_trans %>%
#   .[['Load']]
# svm_caret <- caret::train(input_x, input_y,
#                           method = 'svmRadial',
#                           tuneGrid = this_grid,
#                           preProc = NULL,
#                           metric = 'RMSE',
#                           maximize = FALSE,
#                           trControl = svm_ctrl)
# svmRadial <- predict(svm_caret, test_x_trans)
# svmRadial <- as.matrix(retransform_load(svmRadial, pre_proc_values))
# colnames(svmRadial) <- 'svmRadial'
# # svmRadial <- readRDS('../../extdata/expert_svm.rds')
# server_data$list_expert$ml_expert$svmRadial <- svmRadial
#
#
# this_grid <- expand.grid(degree = 2,
#                          scale = 0.005,
#                          C = 120)
# svm_caret <- caret::train(input_x, input_y,
#                           method = 'svmPoly',
#                           tuneGrid = this_grid,
#                           preProc = NULL,
#                           metric = 'RMSE',
#                           maximize = FALSE,
#                           trControl = svm_ctrl)
# pred_svm_caret <- predict(svm_caret, test_x_trans)
# svmPoly <- as.matrix(retransform_load(pred_svm_caret, pre_proc_values))
# colnames(svmPoly) <- 'svmPoly'
# # svmPoly <- readRDS('../../extdata/expert_svmPoly.rds')
# server_data$list_expert$ml_expert$svmPoly <- svmPoly
#
#
# input_x <- server_data$train_data %>%
#   select(-Load)
# input_y <- server_data$train_data %>%
#   select(Load)
# test_x <- server_data$test_data %>%
#   select(-Load)
# ppr <- ppr(x = input_x, y = input_y,
#            nterms = 3, max.terms = 4,
#            sm.method = 'spline')
# ppr <- as.matrix(predict(ppr, test_x), ncol = 1)
# colnames(ppr) <- 'ppr'
# # ppr <- readRDS('../../extdata/expert_ppr.rds')
# server_data$list_expert$ml_expert$ppr <- ppr
#
#
# # rfexp <- ranger::ranger(Load ~ ., mtry = 6,
# #                         num.trees = 1500,
# #                         min.node.size = 4,
# #                         seed = 19,
# #                         data = server_data$train_data,
# #                         quantreg = TRUE)
# # pred_rf <- predict(rfexp, server_data$test_data)
# # preds <- predict(rfexp, server_data$test_data,
# #                  quantiles = seq(0.05, 0.95, 0.1),
# #                  type = 'quantiles')
# # rf <- data.matrix(cbind(pred_rf$predictions,
# #                         preds$predictions))
# # colnames(rf) <- c('randomForest',
# #                   paste0('rf_', seq(0.05, 0.95, 0.1)))
# # server_data$list_expert$ml_expert$rf <- rf
#
# server_data$list_expert$ml_expert <- do.call(cbind,
#                                              server_data$list_expert$ml_expert)
#
#
# plot_metrics_expert(server_data$test_data,
#                     server_data$list_expert,
#                     server_data$orig_expert$rmse)
#
#
# experts <- do.call(cbind, server_data$list_expert)
# experts <- cbind(experts, server_data$orig_expert$forecast)
# colnames(experts)[ncol(experts)] <- 'orig'
# m <- mixture(Y = server_data$test_data$Load, experts, model = 'MLpol', loss.type = "square", loss.gradient = T)
# plot(m)
# rmse(server_data$test_data$Load, m$prediction)
# # 1037.788
# # 1037.614 plus gam orig
#
# or <- oracle(Y=server_data$test_data$Load, experts=experts, model="convex", loss.type="square")
# plot(sort(or$coefficients, decreasing=T), type="b")
# to_plot <- data.frame(id = seq(1:ncol(experts)), coef = sort(or$coefficients, decreasing=T))
# ggplot(to_plot) +
#   geom_point(aes(x = id, y = coef))
# o <- order(or$coefficients, decreasing=T)
# colnames(experts)[o]

