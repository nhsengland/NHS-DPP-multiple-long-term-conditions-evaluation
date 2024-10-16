#################################
## NHS DPP evaluation          ##
##                             ##
## =========================== ##
##                             ##
##  Author: Stefano conti      ##
##  (e. stefano.conti@nhs.net) ##
##                             ##
#################################

##############
## Preamble ##
##############

rm(list=ls())  # Clear work-space


project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation"  # Set project directory name


load(file.path(project_dir, 
               "R/data/19_April/cleaned_data_for_pred_prob.RData")
     )  # Load cleaned analysis data-frame


#########################
## Data Pre-processing ##
#########################

df_1 <- within(df_out, expr={
  Person_Id <- as.character(Person_Id)  # Format "Person_Id" as character
  
  pop_group <- factor(pop_group)  # Format "pop_group" as factor
  
  IMD_DECILE_PERSON <- factor(IMD_DECILE_PERSON)  # Format "IMD_DECILE_PERSON" as factor
  
  rural_urban_GP <- factor(rural_urban_GP)  # Format "rural_urban_GP" as factor
  
  IMD_quintile <- factor(IMD_quintile)  # Format "IMD_quintile" as factor
  
  IMD_quintile_GP <- factor(IMD_quintile_GP)  # Format "IMD_quintile_GP" as factor
  
  QOF_overall_quintile_GP <- factor(QOF_overall_quintile_GP)  # Format "QOF_overall_quintile_GP" as factor
  
  size_quintile_GP <- factor(size_quintile_GP)  # Format "size_quintile_GP" as factor
  
  fte_quintile_GP <- factor(fte_quintile_GP)  # Format "fte_quintile_GP" as factor
  
  h_date_quarter <- factor(h_date_quarter)  # Format "h_date_quarter" as factor
  
  LTC_sum_conditions <- as.integer(LTC_sum_conditions)  # Format "LTC_sum_conditions" as integer
  })

df_1[grep("^LTC", x=names(df_1))] <- lapply(df_1[grep("^LTC", x=names(df_1))], 
                                            FUN=as.integer
                                            )  # Format LTC diagnosis variables as integers


#############################
## Modelling the odds of   ##
## NHS DPP completion from ##
## programme non-starters  ##
#############################

  
predict_vec <- setdiff(c("age", "gender", "ethnicity", "ccg_GP", "h_date_quarter", "rural_urban_GP", 
                         grep("quintile", x=names(df_1), value=TRUE), "arrivals", "admissions", "appointments",
                         grep("^LTC", x=names(df_1), value=TRUE)
                         ), 
                       y=c("LTC_condition_count", "LTC_Healthy_Well", "LTC_sum_conditions")
                       )  # Set vector of predictors names

complete_frm <- as.formula(paste("pop_group", 
                                 paste(predict_vec, collapse=" + "), 
                                 sep=" ~ "
                                 )
                           )  # Derive GLM formula for NHS DPP completion



complete_fit <- step(glm(update(complete_frm, new=. ~ . - ccg_GP),   # N.B.: data sparsity by population subgroup across CCGs required exclusion of 'ccg_GP' predictor
                         family=binomial(link="logit"), 
                         data=droplevels(subset(df_1, subset=pop_group %in% c(2, 4)))
                         ), 
                     direction="both"
                     )  # Fit step-wise Logistic regression model to NHS DPP completion from NHS DPP non-starters


#Save model

saveRDS(complete_fit, paste(project_dir,"/R/data/19_April/complete_fit.rds", sep = ''))

#Read in to save time
#complete_fit <- readRDS(paste(project_dir,"/R/data/19_April/complete_fit.rds", sep = ''))

##############################
## Predicting the odds of   ##
## NHS DPP completion among ##
## programme non-referrals  ##
##############################

prd_complete_vec <- predict(complete_fit, 
                            newdata=df_1, 
                            type="response"
                            )  # Derive vector of predicted NHS DPP completion probabilities from NHS DPP non-starters

df_2 <- cbind(df_1, 
              pred_prob_completion_given_1=prd_complete_vec
              )  # Augment cleaned analysis data-frame with predicted probabilities vector


save(complete_fit, df_2, 
     file=file.path(project_dir, "R/data/19_April/data_for_matching.RData")
     )  # Save augmented cleaned analysis data-frame"
