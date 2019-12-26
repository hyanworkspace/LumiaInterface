library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyr)
# library(R39Toolbox)
library(ggplot2)
library(mgcv)
library(qgam)
library(opera)
library(plotly)
library(xgboost)
library(magrittr)
library(forecast)
library(RColorBrewer)
library(yarrr)
library(dummies)
library(ranger)
library(shinyBS)
library(caret)
options(shiny.maxRequestSize = 90 * 1024 ^ 2)
options(digits = 4)
options(warn = -1)
# Functions ----------------------------------------
# for (i in list.files(path = "../../../R/", full.names = TRUE)) {
#   if(!stringr::str_detect(i, 'demo')) source(file = i, encoding = "UTF-8")
# }
for (i in list.files(path = "./func/", full.names = TRUE)) {
  source(file = i, encoding = "UTF-8")
}

# Modules ------------------------------------------


# ------- Fixed parameters --------------
tabs.content <- list(
  # ------ Constant bias params ---------
  list(Title = "Constant_bias", Content = tagList(
    numericInput("n_expert_cb",
                 label = h5("Number of experts : "),
                 value = 10),
    numericInput("amplitude_cb",
                 label = h5("Amplitude : "),
                 value = 2),
    fileInput("pretrained_cb",
              label = h5("Pretrained experts (.rds):"))
  )),
  # ------ Bagging params ---------
  list(Title = "Bagging", Content = tagList(
    numericInput("n_expert_bagg",
                 label = h5("Number of experts : "),
                 value = 10),
    sliderInput("sr_bagg",
                label = h5("Sampling rate :"),
                min = 0,
                max = 1,
                value = 0.8),
    fileInput("pretrained_bagg",
              label = h5("Pretrained experts (.rds):"))
  )),
  # ------ Boosting params ---------
  list(Title = "Boosting", Content = tagList(
    numericInput("n_expert_boost",
                 label = h5("Number of experts : "),
                 value = 10),
    sliderInput("lambda_boost",
                label = h5("Lambda :"),
                min = 0,
                max = 1,
                value = 0.1),
    fileInput("pretrained_boost",
              label = h5("Pretrained experts (.rds):"))
  )),
  # ------ Rw params ---------
  list(Title = "Random walk", Content = tagList(
    numericInput("n_expert_rw",
                 label = h5("Number of experts : "),
                 value = 10),
    numericInput("var_fact_rw",
                 label = h5("Variance factor : "),
                 value = 4),
    fileInput("pretrained_rw",
              label = h5("Pretrained experts (.rds):"))
  )),
  # ------ Qgam params ---------
  list(Title = "QGam", Content = tagList(
    # numericInput("n_expert_qgam",
    #              label = h5("Number of experts : "),
    #              value = 10),
    fileInput("pretrained_qgam",
              label = h5("Pretrained experts (.rds):"))
  )),
  # ------ Gamlss params ---------
  list(Title = "Gamlss", Content = tagList(
    # numericInput("n_expert_gamlss",
    #              label = h5("Number of experts : "),
    #              value = 10),
    fileInput("pretrained_gamlss",
              label = h5("Pretrained experts (.rds):"))
  ))
)
