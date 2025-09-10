library(pbapply)
library(rlang)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(DALEX)
library(readr)
library(gbm)
library(dplyr)
library(caret)
library(ggplot2)
library(pROC)
library(rms)
library(rmda)
library(dcurves)
library(Hmisc)
library(ResourceSelection)
library(DynNom)
library(survey)
library(caret)
library(foreign)
library(plotROC)
library(survival)
library(shapper)
library(iml)
library(e1071)
library(ROCR)
library(corrplot)
library(lattice)
library(Formula)
library(SparseM)
library(survival)
library(riskRegression)
library(pheatmap)
library(fastshap)
library(naivebayes)
library(ingredients)
library(mlr3)
library(table1)
library(tableone)
library(adabag)
library(RColorBrewer)
library(VIM)
library(mice)
library(autoReg)
library(cvms)
library(tibble)
library(plotROC)
library(pROC)
library(ggplot2)
library(cvms)
library(tibble)
library(corrplot)
library(data.table)
library(pheatmap)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(ROSE)
library(DMwR)
library(scales)
library(catboost)
library(lightgbm)
library(plotROC)
library(pROC)
library(ggplot2)
library(kernelshap)
library(shapviz)
setwd("C:\\Users\\25297\\Desktop\\XXX")
data=read.csv("data.csv",header = T,encoding = "GBK")
colnames(data)
data$Result = factor(data$Result,levels = c(0,1),labels = c('No','Yes'))  
data$sex = factor(data$sex,levels = c(0,1),labels = c('Female','Male'))
data$Marital_Status = factor(data$Marital_Status,levels = c(1,2,3),labels = c('Married','Single','Other'))
data$in_hospital_way = factor(data$in_hospital_way,levels = c(0,1),labels = c('ED','OPD'))
set.seed(52)
inTrain = createDataPartition(y=data[,"Result"], p=0.7, list=F)
traindata = data[inTrain,]
testdata = data[-inTrain,]
write.csv(traindata,"dev.csv",row.names = F)
write.csv(testdata,"vad.csv",row.names = F)
x = traindata   
x1 = colnames(x[,16:ncol(x)])
x2 = colnames(x[,2:15])

CreateTableOne(data=x)

myVars = colnames(x[,2:ncol(x)])

catVars = colnames(x[,2:15])
tab2 = CreateTableOne(vars = myVars,data = x,factorVars = catVars)

print(tab2,format0ptions=list(big.showAllLevels=TRUE,mark=','))

tab2Mat = print(tab2,quote = FALSE,noSpaces = TRUE,printToggle = FALSE)
write.csv(tab2Mat,file = 'Tableone.csv')


tab3 = CreateTableOne(vars = myVars,strata = 'Result',
                       data = x,factorVars = catVars)

print(tab3,showAllLevels=TRUE,format0ptions=list(big.mark=','))


tab3Mat = print(tab3,quote = FALSE,noSpaces = TRUE,
                 printToggle = FALSE)
write.csv(tab3Mat,file = 'uni.csv')


install.packages("autoReg")
overall.log = glm(Result ~ .,data=x,family=binomial) 
summary(overall.log)

model3 = autoReg(overall.log,uni=TRUE,milti=TRUE,threshold=0.05)
model3
write.csv(model3,"Logistic.csv",row.names = F)


colnames(x)

var=c("Result",
      "sex","CCI","qSOFA","T")#




data=read.csv("final.csv",header = T,encoding = "GBK")
colnames(data)

data$Result = factor(data$Result,levels = c(0,1),labels = c('No','Yes'))

set.seed(52)
inTrain = createDataPartition(y=data[,"Result"], p=0.7, list=F)
traindata = data[inTrain,]
testdata = data[-inTrain,]


dev = traindata
vad = testdata


dev = dev[,var]
vad = vad[,var]
dev$Result = factor(as.character(dev$Result))


models = c("glm","svmRadial","gbm","nnet","extraTrees","xgbTree","kknn","AdaBoost.M1")


models_names = list(Logistic="glm",SVM="svmRadial",GBM="gbm",NeuralNetwork="nnet",RandomForest="extraTrees",Xgboost="xgbTree",KNN="kknn",Adaboost="AdaBoost.M1")#


glm.tune.grid = NULL
svm.tune.grid = expand.grid(sigma = 0.001, C = 0.09)
gbm.tune.grid = expand.grid(n.trees = 100, interaction.depth = 5,shrinkage = 0.1, n.minobsinnode = 30)
nnet.tune.grid = expand.grid(size = 6,decay = 0.6)
rf.tune.grid = expand.grid(mtry = 11,numRandomCuts = 3)
xgb.tune.grid = expand.grid(nrounds = 10,max_depth = 3,eta = 0.001,
                            gamma = 0.5,colsample_bytree = 0.5,min_child_weight = 1,subsample = 0.6)
knn.tune.grid <- expand.grid(kmax = 12 ,distance = 1,kernel = "optimal")
ada.tune.grid <- expand.grid(mfinal = 2,maxdepth = 2,coeflearn = "Zhu")

Tune_table = list(glm = glm.tune.grid,
                  svmRadial = svm.tune.grid,
                  gbm = gbm.tune.grid,
                  nnet = nnet.tune.grid,
                  extraTrees = rf.tune.grid,
                  xgbTree = xgb.tune.grid,
                  kknn = knn.tune.grid,
                  AdaBoost.M1 = ada.tune.grid
)


train_probe = data.frame(Result = dev$Result)
test_probe = data.frame(Result = vad$Result)


importance = list()


ML_calss_model = list()

set.seed(520)
train.control <- trainControl(method = 'repeatedcv',
                              number = 10, 
                              repeats = 5, 
                              classProbs = TRUE, 
                              summaryFunction = twoClassSummary)

pb = txtProgressBar(min = 0, max = length(models), style = 3)
for (i in seq_along(models)) {
  model <- models[i]
  model_name <- names(models_names)[which(models_names == model)]  
  set.seed(52)
  fit = train(Result~.,
              data = dev,
              tuneGrid = Tune_table[[model]],
              metric='ROC',
              method= model,
              trControl=train.control)
  
  train_Pro = predict(fit, newdata = dev, type = 'prob')
  test_Pro = predict(fit, newdata = vad, type = 'prob')
  
  train_probe[[model_name]] <- train_Pro$Yes
  test_probe[[model_name]] <- test_Pro$Yes
  
  ML_calss_model[[model_name]] = fit  # Store model with name
  importance[[model_name]] = varImp(fit, scale = TRUE)  # Store importance with name
  
  setTxtProgressBar(pb, i)
}
close(pb)  




#9.LightGBM
train = dev
train$Result = ifelse(train$Result=="Yes",1,0)
dtrain = lgb.Dataset(as.matrix(train[2:ncol(train)]), label = train$Result)
test = vad[,var]
test$Result = ifelse(test$Result=="Yes",1,0)
dtest = lgb.Dataset.create.valid(dtrain, as.matrix(test[2:ncol(test)]), label = test$Result)
params = list(
  objective = "binary", 
  metric = "auc", 
  min_data = 1L, 
  learning_rate = 1.0, 
  num_threads = 2L,
  force_col_wise = T)
valids = list(test = dtest)
lightgbm_model = lgb.train(params = params,data = dtrain,
                           nrounds = 5L, 
                           valids = valids, 
                           early_stopping_rounds = 3L)

train_probe$LightGBM = predict(lightgbm_model,newdata = as.matrix(dev[2:ncol(dev)]),type = 'prob')
test_probe$LightGBM = predict(lightgbm_model,newdata = as.matrix(vad[2:ncol(vad)]),type = 'prob')

lightGBM_Imp = lgb.importance(lightgbm_model, percentage = TRUE)
write.csv(lightGBM_Imp,"LightGBM_important.csv",row.names = F)

rt=read.csv("LightGBM_important.csv", header=T,check.names=F)
g <- ggplot(rt, aes(x=Gain,y=reorder(Feature,Gain)))
p2=g+geom_bar(aes(fill=Gain),stat = "identity",
              width = 0.6,position = position_stack(reverse = TRUE),size=1)+
  theme_classic()+
  scale_fill_gradient()+
  theme(plot.title = element_text(hjust = 0.5,size = 16),
        legend.position = "none",
        axis.text=element_text(size=10,face = "bold",color = "black"),
        axis.title.x = element_text(size = 12,face = "bold",color = "black"),
        axis.title.y = element_text(size = 12,face = "bold",color = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))+
  labs(x="Importance Scores",y = "Features",title = "LightGBM")
p2
pdf("LightGBM_importance.pdf",7,5,family = "serif")
p2
dev.off()
##############################################################################################


##############################################################################################
#10.CatBoost
train = dev
train$Result = ifelse(train$Result=="Yes",1,0)
train <- as.data.frame(lapply(train, function(x) {
  if (is.integer(x)) {
    return(as.numeric(x))
  }
  return(x)
}))

train_pool = catboost.load_pool(as.matrix(train[2:ncol(train)]),label = train$Result)
test = vad[,var]
test$Result = ifelse(test$Result=="Yes",1,0)

test <- as.data.frame(lapply(test, function(x) {
  if (is.integer(x)) {
    return(as.numeric(x))
  }
  return(x)
}))
test_pool = catboost.load_pool(as.matrix(test[2:ncol(test)]),label =  test$Result)
fit_params = list(
  iterations = 100,
  use_best_model = TRUE,
  eval_metric = 'AUC',
  ignored_features = c(4, 9),
  border_count = 32,
  depth = 5,
  learning_rate = 0.03,
  random_seed =123)
Catboost_model = catboost.train(train_pool, test_pool, fit_params)
Catboost_model


train_probe$CatBoost = catboost.predict(Catboost_model, train_pool, prediction_type = 'Probability')
test_probe$CatBoost = catboost.predict(Catboost_model, test_pool, prediction_type = 'Probability')

Catboost_Imp = catboost.get_feature_importance(Catboost_model)
Catboost_Imp = data.frame(Feature = colnames(dev)[2:ncol(dev)],Overall = Catboost_Imp)
write.csv(Catboost_Imp,"CatBoost_important.csv",row.names = F)

rt=read.csv("CatBoost_important.csv", header=T,check.names=F)
g <- ggplot(rt, aes(x=Overall,y=reorder(Feature,Overall))) 
p2=g+geom_bar(aes(fill=Overall),stat = "identity",
              width = 0.6,
              position = position_stack(reverse = TRUE),
              size=1)+
  theme_classic()+
  scale_fill_gradient()+
  theme(plot.title = element_text(hjust = 0.5,size = 16),
        legend.position = "none",
        axis.text=element_text(size=10,
                               face = "bold",
                               color = "black"),
        axis.title.x = element_text(size = 12,
                                    face = "bold",
                                    color = "black"),
        axis.title.y = element_text(size = 12,
                                    face = "bold",
                                    color = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))+
  labs(x="Importance Scores",y = "Features",title = "CatBoost")
p2
pdf("CatBoost_importance.pdf",7,5,family = "serif")
p2
dev.off()



for(model_name in names(models_names)){

  imp = importance[[model_name]]

  imp_table <- as.data.frame(imp$importance)
  imp_table$Features <- rownames(imp_table)

  if ("Yes" %in% colnames(imp_table)) {
    fill_col <- "Yes"
  } else if ("Overall" %in% colnames(imp_table)) {
    fill_col <- "Overall"
  } else {
    stop("Neither 'Yes' nor 'Overall' column found in importance table.")
  }

  g = ggplot(imp_table, aes(x = !!sym(fill_col), y = reorder(Features, !!sym(fill_col))))
  p2 = g + geom_bar(aes(fill = !!sym(fill_col)), stat = "identity", width = 0.6, position = position_stack(reverse = TRUE), size = 1) +
    theme_classic() + scale_fill_gradient() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "none",
          axis.text = element_text(size = 10, face = "bold", color = "black"),
          axis.title.x = element_text(size = 12, face = "bold", color = "black"),
          axis.title.y = element_text(size = 12, face = "bold", color = "black"),
          legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
    labs(x = "Importance Scores", y = "Features", title = paste0(model_name, " "))
  
  pdf(paste0(model_name, "_important.pdf"), 7, 5, family = "serif")
  print(p2)
  dev.off()
}



models_names = list(Logistic="glm",SVM="svmRadial",GBM="gbm",NeuralNetwork="nnet",RandomForest="extraTrees",
                    Xgboost="xgbTree",KNN="kknn",Adaboost="AdaBoost.M1",
                    LightGBM = "LightGBM",CatBoost = "CatBoost")#
########################################################################################################################
########################################################################################################################
Train = train_probe
Test = test_probe


datalist = list(Train= train_probe,
                Test= test_probe)

for (newdata_tt in names(datalist)) {
  
  newdata = datalist[[newdata_tt]]

  formula = as.formula(paste0("Result ~ ", paste(colnames(newdata)[2:ncol(newdata)], collapse = " + ")))
  trellis.par.set(caretTheme())
  cal_obj = calibration(formula, data = newdata, class = 'Yes',cuts = 3)
  caldata=as.data.frame(cal_obj$data)
  caldata=na.omit(caldata)

  Calibrat_plot=ggplot(data = caldata,aes(x=midpoint,
                            y=Percent,
                            group = calibModelVar,
                            color=calibModelVar))+
    geom_point(size=1)+
    geom_line(linewidth=0.65)+
    geom_abline(slope = 1, intercept = 0 ,color="black",linetype = 'dotdash')+
    xlab("Bin Midpoint")+
    ylab("Observed Event Percentage")+
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
      axis.text=element_text(size=12,face="bold"),
      legend.position=c(0.9,0.3),
      legend.background = element_blank(),
      axis.title.y = element_text(size=12,face="bold"),
      axis.title.x = element_text(size=12,face="bold"),
      panel.border = element_rect(color="black",size=1),
      panel.background = element_blank())+
    scale_color_discrete(name = "Model")
  pdf(paste0(newdata_tt,"Calibration.pdf"),5,5,family = "serif")
  print(Calibrat_plot)
  dev.off()
  
  
  ROC_list = list()
  ROC_label = list()
  AUC_metrics = data.frame()
  Evaluation_metrics = data.frame(Model = NA,Threshold=NA,Accuracy=NA,Sensitivity=NA,Specificity=NA,Precision=NA,F1=NA)

  for (model_name in names(models_names)) {
    
    ROC = roc(response=newdata$Result,predictor=newdata[,model_name])
    AUC = round(auc(ROC),3)
    CI = ci.auc(ROC)
    label = paste0(model_name," (AUC=",sprintf("%0.3f", AUC),",95%CI:",sprintf("%0.3f", CI[1]),"-",sprintf("%0.3f", CI[3]),")")
    
    bestp = ROC$thresholds[
      which.max(ROC$sensitivities+ROC$specificities-1)
    ]
    
    predlab = as.factor(ifelse(newdata[,model_name] > bestp,"Yes","No"))
    
    index_table = confusionMatrix(data = predlab,
                                  reference = newdata$Result,
                                  positive = "Yes",
                                  mode="everything")
    
    mydata = data.frame(reference=newdata$Result,prediction=predlab)
    mytibble =  as.tibble(table(mydata))
    
    confusion_plot = plot_confusion_matrix(mytibble,
                                           target_col = "reference",
                                           prediction_col = "prediction",
                                           counts_col = "n",
                                           sub_col = NULL,
                                           class_order = NULL,
                                           add_sums = FALSE,
                                           add_counts = TRUE,
                                           add_normalized = TRUE,
                                           add_row_percentages = F,
                                           add_col_percentages = F,
                                           diag_percentages_only = FALSE,
                                           rm_zero_percentages = TRUE,
                                           rm_zero_text = TRUE,
                                           add_zero_shading = TRUE,
                                           amount_3d_effect = 1,
                                           add_arrows = TRUE,
                                           counts_on_top = FALSE,
                                           palette = "Blues",
                                           intensity_by = "counts",
                                           intensity_lims = NULL,
                                           intensity_beyond_lims = "truncate",
                                           theme_fn = ggplot2::theme_minimal,
                                           place_x_axis_above = TRUE,
                                           rotate_y_text = TRUE,
                                           digits = 1,
                                           font_counts = font(),
                                           font_normalized = font(),
                                           font_row_percentages = font(),
                                           font_col_percentages = font(),
                                           arrow_size = 0.048,
                                           arrow_nudge_from_text = 0.065,
                                           tile_border_color = NA,
                                           tile_border_size = 0.1,
                                           tile_border_linetype = "solid",
                                           sums_settings = sum_tile_settings(),
                                           darkness = 0.8)
    
    pdf(paste0(newdata_tt,model_name,"_cm_plot.pdf"),5,5,family = "serif")
    print(confusion_plot)
    dev.off()
    Evaluation_metrics = rbind(Evaluation_metrics,c(Model = model_name, Threshold = bestp,  
                                                    Accuracy = sprintf("%0.3f",index_table[["overall"]][["Accuracy"]]),
                                                    Sensitivity = sprintf("%0.3f",index_table[["byClass"]][["Sensitivity"]]),
                                                    Specificity = sprintf("%0.3f",index_table[["byClass"]][["Specificity"]]),
                                                    Precision = sprintf("%0.3f",index_table[["byClass"]][["Precision"]]),
                                                    F1 = sprintf("%0.3f",index_table[["byClass"]][["F1"]])  )
    )
    
    ROC_label[[model_name]] = label
    ROC_list[[model_name]] = ROC
    
  }
  write.csv(Evaluation_metrics,paste0(newdata_tt,"_Evaluation_metrics.csv"),row.names = F)
  
  
  
  ROC_plot=pROC::ggroc(ROC_list,size=1.5,legacy.axes = T)+theme_bw()+
    labs(title = ' ROC curve')+
    theme(plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
          axis.text=element_text(size=12,face="bold"),
          legend.title = element_blank(),
          legend.text = element_text(size=12,face="bold"),
          legend.position=c(0.7,0.25),
          legend.background = element_blank(),
          axis.title.y = element_text(size=12,face="bold"),#element_blank()
          axis.title.x = element_text(size=12,face="bold"),
          panel.border = element_rect(color="black",size=1),
          panel.background = element_blank())+
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),colour='grey',linetype = 'dotdash')+
    scale_colour_discrete(
      breaks=c(names(models_names)),
      labels=c(ROC_label))
  pdf(paste0(newdata_tt,"_ROC.pdf"),7,7,family = "serif")
  print(ROC_plot)
  dev.off()
  
  dca_data = newdata
  dca_data$Result=ifelse(dca_data$Result=="Yes",1,0)
  
  DCA_list = list()
  for (model_name in names(models_names)) {
    dca_formula = as.formula(paste("Result ~", model_name))
    set.seed(123)
    dca_curvers = decision_curve(dca_formula, 
                                 data = dca_data, 
                                 study.design = "cohort", 
                                 bootstraps = 50 
    )
    DCA_list[[model_name]] = dca_curvers
  }
  
  dca = setNames(DCA_list, names(DCA_list))

  pdf(paste0(newdata_tt,"DCA.pdf"),7,7,family = "serif")
  plot_decision_curve(dca, curve.names = c(names(models_names)),
                      cost.benefit.axis = F, 
                      confidence.intervals = "none" ,
                      lwd = 2,
                      legend.position ="topright")+theme(
                        plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
                        axis.text=element_text(size=12,face="bold"),
                        legend.title = element_blank(),
                        legend.text = element_text(size=12,face="bold"),
                        #legend.position="topright",
                        legend.background = element_blank(),
                        axis.title.y = element_text(size=12,face="bold"),#element_blank()
                        axis.title.x = element_text(size=12,face="bold"),
                        panel.border = element_rect(color="black",size=1),
                        panel.background = element_blank())
  dev.off()
  write.csv(dca_data,paste0(newdata_tt,"_PRplot.csv"),row.names = F)
}



ML_calss_model$LightGBM = lightgbm_model
ML_calss_model$CatBoost = Catboost_model



n_train = 187 
n_test = 80   
names(models_names)


best_Model = "RandomForest"


explain_kernel = kernelshap(ML_calss_model[[best_Model]], dev[1:n_train,-1], bg_X = vad[1:n_test,-1])  

shap_value = shapviz(explain_kernel,X_pred = dev[1:n_train,-1], interactions = TRUE) 

pdf(paste0("SHAP_",best_Model,"_sv_force.pdf"),7,5)
sv_force(shap_value$Yes, row_id = 12,size = 9)+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()

pdf(paste0("SHAP_",best_Model,"_importance_beeswarm.pdf"),7,5)
sv_importance(shap_value$Yes, kind = "beeswarm", 
              viridis_args = list(begin = 0.25, end = 0.85, option = "B"),#A-H
              show_numbers = F)+
  ggtitle(label = paste0("",best_Model))+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()


pdf(paste0("SHAP_",best_Model,"_importance_bar.pdf"),7,5)
sv_importance(shap_value$Yes, kind = "bar", show_numbers = F,
              fill = "#fca50a",
              class = "Yes")+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()

colnames(dev)

pdf(paste0("SHAP_",best_Model,"_dependence.pdf"),5,5)
sv_dependence(shap_value$Yes,v = "CCI",
              color = "#3b528b",
              color_var = "T",
)+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()


pdf(paste0("SHAP_",best_Model,"_waterfall.pdf"),5,5)
sv_waterfall(shap_value$Yes, row_id = 12,
             fill_colors = c("#f7d13d", "#a52c60"))+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()

sv_interaction(shap_value$Yes,
               kind = "beeswarm",
               max_display = 5,
               alpha = 0.3,
               bee_width = 0.3,
               bee_adjust = 0.5,
               viridis_args = getOption("shapviz.viridis_args"),
               color_bar_title = "Row feature value")

