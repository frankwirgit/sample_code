# An example for model training based on K-fold methods in the caret package

rm(list = ls())
set.seed(10)
require(caret)
require(tidyr)
require(dplyr)
require(e1071)
#require(doMC)
require(doParallel)
require(pROC)
#registerDoMC(cores = 5)

cl<- makeCluster(10)
registerDoParallel(cl)

source_path<-'/homes/hny5/lcao/R/dev'

source(paste0(source_path, '/data_funcs.R'))
source(paste0(source_path, '/preprocess_funcs.R'))
source(paste0(source_path, '/postprocess_funcs.R'))

in_basepath <- "/homes/hny5/lcao/R/data/inputs"
out_basepath <- "/homes/hny5/lcao/R/data/inputs/outputs"

groups  <- read.csv(paste(in_basepath, "patient_groupings_bolus_demo.csv", sep="/"))
all_groupids <- unique(groups$groupid)
#all_groupids  <- c(1,5,8)
ngroups  <- length(all_groupids)

## full_hypo_data <- create_dframes(paste0(in_basepath,"ClassFiles"), pos_file = "PositiveClass-WithBWHypoCountsSG.txt", neg_file = "NegativeClass-WithBWHypoCountsSG.txt")
time_horizon <- "4hr"
full_hypo_data_original <- create_dframes_singlefile(paste0(in_basepath), class_file = "/Labelled-WithBWHypoCountsSG.txt", horizon = time_horizon)
drop <- grep("Bolus", colnames(full_hypo_data_original))
full_hypo_data<-full_hypo_data_original[,-drop]

test_aucs <- rep(NA, ngroups)
train_aucs <- rep(NA, ngroups)
train_sizes <- rep(NA, ngroups)
group_sizes <- rep(NA, ngroups)


for (i in (1:ngroups)) {
#foreach(i=(1:ngroups), .combine=cbind, .packages=c("dplyr", "lubridate", "caret")) %dopar% {
    i_groupid <- all_groupids[i]
    cat("Modeling group :", i_groupid, "\n")
    
    out_prefix  <- paste0('Group-', i_groupid, "-", time_horizon, "_")
    out_filebase <- paste(out_basepath, out_prefix, sep="/")
    
    ## group_users <- groups %>% filter(groupid == i_groupid) %>% select(userid)
    ## group_sizes[i] <- length(group_users$userid)
    
    ## cat("Number of users in group:", nrow(group_users), "\n")
    ## sub_hypo_data <- filter(full_hypo_data, userid %in% group_users$userid)
    
    group_epochs <- groups %>% filter(groupid == i_groupid) %>% select(epoch)
    group_sizes[i] <- length(group_epochs$epoch)
    
    cat("Number of epochs in group:", nrow(group_epochs), "\n")
    sub_hypo_data <- filter(full_hypo_data, epoch %in% group_epochs$epoch)
    
    ok_sub_hypo_data <- sub_hypo_data[complete.cases(sub_hypo_data), ]
    cat("frac. of rows lost after extracting compelete cases:", (nrow(sub_hypo_data) - nrow(ok_sub_hypo_data)) / nrow(sub_hypo_data), "\n")

    if (sum(ok_sub_hypo_data$label == "hypo") < 5 || sum(ok_sub_hypo_data$label == "nonhypo") < 50) {
        test_aucs[i] <- NA
        print("Skipping. Not enough data")
        next
    }
    
    sub_hypo_data <- ok_sub_hypo_data
    sub_hypo_data <- add_binned_epoch(sub_hypo_data)

    cat("Number of rows in sampled hypo data:", nrow(sub_hypo_data), "\n")
    print(table(sub_hypo_data$label))

    hypo_data <- sub_hypo_data
    inTrain <- createDataPartition(hypo_data$label, p = .80,  list=FALSE)

    train_split <- hypo_data[inTrain, ]
    test_split <- hypo_data[-inTrain, ]

    train_split <- select(train_split, -c(userid, epoch))
    test_retattr <- select(test_split, c(userid, epoch))
    test_split <- select(test_split, -c(userid, epoch))

    nhypo_train <- sum(train_split$label == "hypo")
    nnonhypo_train <- sum(train_split$label == "nonhypo")
        
    if (nhypo_train < nnonhypo_train) {
        train_split <- balance_by_hypo(train_split)$balanced_data
    }
    
    #train_split <- upSample(x = select(train_split, -c(label)), y = train_split$label, yname = "label")
    
    fitControl <- trainControl(method = "repeatedcv",
                               number = 5,
                               ## repeated ten times
                               repeats = 1,
                               verboseIter=T,
                               classProbs=T,
                               summaryFunction=twoClassSummary)
    
    rfGrid <- expand.grid(mtry = c(2, 4, 7, 11, 20))
    
    cat("Training started:\n")
    
    modelFit <- train(label ~ ., data = train_split,
                      method = "rf",
                      trControl = fitControl,
                      verbose=T,
                      tuneGrid=rfGrid,
                      metric = "ROC")

    modelImp <- varImp(modelFit)
    
    train_prob_pred  <- predict(modelFit, newdata = train_split, type = "prob")[,1]
    train_roc <- roc(train_split$label, train_prob_pred)
    train_sizes[i] <- nrow(train_split)
    cat("AUC on train data:", train_roc$auc, "\n")
    train_aucs[i] <- train_roc$auc
    
    if (all(table(test_split$label) > 0)) {
        prob_pred_test  <- predict(modelFit, newdata = test_split, type = "prob")[,1]
        roc_test <- roc(test_split$label, prob_pred_test)
        cat("AUC on test data:", roc_test$auc, "\n")
        test_aucs[i] <- roc_test$auc

        pdf(paste0(out_filebase, 'modelFit.pdf'))
        print(plot(modelFit))
        dev.off()
        
        pdf(paste0(out_filebase, 'modelImp.pdf'))
        print(plot(modelImp))
        dev.off()
        
        pdf(paste0(out_filebase, 'rocCurve.pdf'))
        plot(roc_test, print.auc=T, grid=c(0.1,0.1))
        dev.off()

        pred_low <- as.factor(ifelse(prob_pred_test > 0.15, "hypo", "nonhypo"))
        pred_med <- as.factor(ifelse(prob_pred_test > 0.5, "hypo", "nonhypo"))
        pred_high <- as.factor(ifelse(prob_pred_test > 0.85, "hypo", "nonhypo"))

        confm_low <- confusionMatrix(pred_low, test_split$label)
        confm_med <- confusionMatrix(pred_med, test_split$label)
        confm_high <- confusionMatrix(pred_high, test_split$label)
        
        stats_summary_file <- paste0(out_prefix, 'stats_summary')
        write_line_tofile("* CM - high Specificity *", stats_summary_file)
        write_tofile(confm_high$table, stats_summary_file)
        
        write_line_tofile("\n\n * CM - medium Specificity *", stats_summary_file)
        write_tofile(confm_med$table, stats_summary_file)
        
        write_line_tofile("\n\n * CM - low Specificity *", stats_summary_file)
        write_tofile(confm_low$table, stats_summary_file)
        
        write_line_tofile('\n\n * Train split *', stats_summary_file)
        write_tofile(table(train_split$label), stats_summary_file)
        write_line_tofile('\n\n * Test split *', stats_summary_file)
        write_tofile(table(test_split$label), stats_summary_file)
        
        
        pred_file <- paste(out_basepath, paste0(out_prefix, 'predictions.csv'), sep="/")
        write.csv(cbind(test_retattr, orig = test_split$label, pred_high = pred_high, pred_med = pred_med, pred_low = pred_low), pred_file, quote=F, row.names=F)
        
        scores_file <- paste(out_basepath, paste0(out_prefix, 'scores.csv'), sep="/")
        write.csv(cbind(test_retattr, orig = test_split$label, score = prob_pred_test), scores_file, quote=F, row.names=F)

        
    } else {
        test_aucs[i] <- NA
    }
}

## group_perf <- data.frame(group_sizes=group_sizes, groupid = all_groupids, test_auc = test_aucs, train_auc = train_aucs, train_size = as.numeric(train_sizes))
## p <- qplot(x = factor(groupid), y = test_auc, data = group_perf, geom="bar", stat="identity", width=0.5)
## p <- p + coord_cartesian(ylim = c(0.5, 0.85))
## p <- p + labs(x="Group ID", y = "AUC") + theme_few()
## ggsave("./plots/auc_per_group.pdf", p, width=8, height=6)

