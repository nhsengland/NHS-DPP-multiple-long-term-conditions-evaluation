# GLM fitting and diagnostic function                                                      
#<    Copyright (C) <2024>  <Stefano Conti>  #

#This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#     see <https://www.gnu.org/licenses/>
# Contact: stefano.conti@nhs.net #


fitGLM_fn <- function(endpointName, 
                      predictorNames_vec, 
                      design_dat, 
                      analysisLabel=NULL, 
                      subgroupLabel=NULL, 
                      outputPath, 
                      interactionLabel=NULL, 
                      interactionName=NULL
                      )
  {
  # Function fits a variety of regression GLMs (i.e. Poisson, Negative Binomial, 
  # Binomial) to a count or binary response variable and computes tables of 
  # customary diagnostic statistics and model inferences.
  # 
  # 
  # Arguments:
  # 
  # endpointName         : character string denoting the response variable's name
  # predictorNames_vec   : character vector denoting explanatory variables' names 
  # design_dat           : design data-frame
  # analysisLabel        : character string denoting the analysis type label
  # subgroupLabel        : character string denoting the subgroup analysis label
  # outputPath           : character string denoting the path to the output 
  #                        destination folder
  # interactionLabel     : character string denoting the "intervention" 
  #                        interaction label
  # interactionName      : character string denoting the name of an explanatory  
  #                        variable interacting with "intervention"
  # 
  # 
  # Output: 
  # 
  # List of named function arguments and ASCII (.csv) tables of diagnostic 
  # statistics and model inferences.
  
  
  # endpointName <- outcome_variable[1]
  # predictorNames_vec <- names_for_matching
  # design_dat <- dat_6M
  # analysisLabel <- "full"
  # subgroupLabel <- "full"
  # outputPath <- file.path(project_dir, "Output/13_March/outcome_modelling/regression/6M")
  # interactionLabel <- NULL
  # interactionName <- NULL
  
  
  ##############
  ## Preamble ##
  ##############
  
  require(lmtest)  # Load library required for Wald-type GLM inferences
  
  require(MASS)  # Load library required for Negative Binomial regression GLM
  
  require(multcomp)  # Load library required for multiple inferences
  
  require(sandwich)  # Load library required for model-robust covariance estimation
  
  
  stopifnot(is.character(endpointName), 
            is.character(predictorNames_vec), 
            is.data.frame(design_dat), 
            is.null(analysisLabel) | is.character(analysisLabel), 
            is.null(subgroupLabel) | is.character(subgroupLabel), 
            is.null(interactionLabel) | is.character(interactionLabel), 
            is.null(interactionName) | is.character(interactionName)
            )
  
  if(! endpointName %in% names(design_dat)) 
    stop("The outcome variable is not in the analysis data-set.")
  
  if(! any(c(predictorNames_vec, interactionName) %in% names(design_dat))) 
    stop("At least one of the predictors to the GLM models is not in the analysis data-set.")
  
  if(is.character(outputPath)) 
    if(! dir.exists(outputPath)) 
      dir.create(outputPath, recursive=TRUE)
  
  
  ########################
  ## Model formulations ##
  ########################
  
  model_out_fly <- if(length(unique(design_dat[[endpointName]])) == 2) 
    setNames("binary", nm="Binomial") else 
      setNames("count", nm="Poisson")  # Set outcome GLM family type
  
  if(model_out_fly == "binary" && 
     ! is.factor(design_dat[[endpointName]])
     ) 
    design_dat[[endpointName]] <- factor(design_dat[[endpointName]])   # Format binary endpoint as factor
  
  
  design_dat <- droplevels(design_dat)  # Drop unused factor levels
  
  
  glm_adj_frm <- as.formula(paste(endpointName, 
                                  paste(c(if(is.character(interactionName)) 
                                    paste("intervention", interactionName, 
                                          sep=" * "
                                          ) else 
                                            "intervention", 
                                    setdiff(predictorNames_vec, 
                                            y=if(is.character(interactionName)) 
                                              interactionName
                                            )), 
                                    collapse=" + "
                                    ), 
                                  sep= " ~ "
                                  )
                            )  # Set formula for adjusted GLM of response with explanatory variables
  
  
  glm_unadj_frm <- update(glm_adj_frm, 
                          new=paste(".", 
                                    paste(".", 
                                          paste("-", setdiff(predictorNames_vec, 
                                                             y=if(is.character(interactionName)) 
                                                               interactionName
                                                             ), 
                                                collapse=" "
                                                ), 
                                          sep=" "
                                          ), 
                                    sep=" ~ "
                                    )
                          )  # Set formula for unadjusted GLM of response with explanatory variables and offset term
  
  
  ###################
  ## Model fitting ##
  ###################
  
  adj_mod_fit <- tryCatch(glm(glm_adj_frm, 
                              family=if(model_out_fly == "count") 
                                poisson(link="log") else 
                                  binomial(link="logit"), 
                              data=design_dat, 
                              offset=if(model_out_fly == "count") 
                                log(studylength), 
                              control=glm.control(maxit=2e2, trace=FALSE)
                              ), 
                          error=function(err) NULL
                          )  # Fit adjusted GLM to endpoint
  
  if("glm" %in% class(adj_mod_fit) && adj_mod_fit$converged) 
    cat("Adjusted", names(model_out_fly), "regression model successfully fitted (AIC =", 
        round(adj_mod_fit$aic, 
              digits=2
              ), 
        "\b) to", endpointName, "in the", analysisLabel, 
        "analysis of the", subgroupLabel, "sample\n\n", 
        sep=" "
        ) else   # Print to terminal AIC of fitted GLM
          cat("Adjusted", names(model_out_fly), "regression model did not converge for", 
              endpointName, "in the", analysisLabel, 
              "analysis of the", subgroupLabel, "sample\n\n", 
              sep=" "
              )  # Print progress message
  
  
  unadj_mod_fit <- tryCatch(glm(glm_unadj_frm, 
                                family=if(model_out_fly == "count") 
                                  poisson(link="log") else 
                                    binomial(link="logit"), 
                                data=design_dat, 
                                offset=if(model_out_fly == "count") 
                                  log(studylength), 
                                control=glm.control(maxit=2e2, trace=FALSE)
                                ), 
                            error=function(err) NULL
                            )  # Fit unadjusted GLM to endpoint
  
  if("glm" %in% class(unadj_mod_fit) && unadj_mod_fit$converged) 
    cat("Unadjusted", names(model_out_fly), "regression model successfully fitted (AIC =", 
        round(unadj_mod_fit$aic, 
              digits=2
              ), 
        "\b) to", endpointName, "in the", analysisLabel, 
        "analysis of the", subgroupLabel, "sample\n\n", 
        sep=" "
        ) else   # Print to terminal AIC of fitted GLM
          cat("Unadjusted", names(model_out_fly), "regression model did not converge for", 
              endpointName, "in the", analysisLabel, 
              "analysis of the", subgroupLabel, "sample\n\n", 
              sep=" "
              )  # Print progress message
  
  
  mod_fit_ls <- list(adjusted=if("glm" %in% class(adj_mod_fit) && adj_mod_fit$converged) 
    adj_mod_fit else 
      unadj_mod_fit, 
    unadjusted=if("glm" %in% class(unadj_mod_fit) && unadj_mod_fit$converged) 
      unadj_mod_fit
    )  # Set list by predictor adjustment type of fitted GLMs
  
  
  if(model_out_fly == "count"){
    nbin_adj_mod_fit <- tryCatch(glm.nb(adj_mod_fit$formula, 
                                        data=design_dat, 
                                        control=glm.control(maxit=2e2, trace=FALSE), 
                                        link="log"
                                        ), 
                                 error=function(err) NULL
                                 )  # Fit adjusted Negative Binomial GLM to endpoint
    
    if("negbin" %in% class(nbin_adj_mod_fit) && nbin_adj_mod_fit$converged) 
      cat("Adjusted Negative Binomial regression model successfully fitted (AIC =", 
          round(nbin_adj_mod_fit$aic, 
                digits=2
                ), 
          "\b) to", endpointName, "in the", analysisLabel, 
          "analysis of the", subgroupLabel, "sample\n\n", 
          sep=" "
          ) else   # Print to terminal AIC of fitted Negative Binomial GLM
            cat("Adjusted Negative Binomial regression model did not converge for", 
                endpointName, "in the", 
                analysisLabel, "analysis of the", 
                subgroupLabel, "sample\n\n", 
                sep=" "
                )  # Print progress message
    
    nbin_unadj_mod_fit <- tryCatch(glm.nb(unadj_mod_fit$formula, 
                                          data=design_dat, 
                                          control=glm.control(maxit=2e2, trace=FALSE), 
                                          link="log"
                                          ), 
                                   error=function(err) NULL
                                   )  # Fit unadjusted Negative Binomial GLM to endpoint
    
    if("negbin" %in% class(nbin_unadj_mod_fit) && nbin_unadj_mod_fit$converged) 
      cat("Unadjusted Negative Binomial regression model successfully fitted (AIC =", 
          round(nbin_unadj_mod_fit$aic, 
                digits=2
                ), 
          "\b) to", endpointName, "in the", analysisLabel, 
          "analysis of the", subgroupLabel, "sample\n\n", 
          sep=" "
          ) else   # Print to terminal AIC of fitted Negative Binomial GLM
            cat("Unadjusted Negative Binomial regression model did not converge for", 
                endpointName, 
                "in the", analysisLabel, 
                "analysis of the", subgroupLabel, 
                "sample\n\n", 
                sep=" "
                )  # Print progress message
    
    
    nbin_mod_fit_ls <- list(adjusted=if("negbin" %in% class(nbin_adj_mod_fit) && nbin_adj_mod_fit$converged) 
      nbin_adj_mod_fit else 
        nbin_unadj_mod_fit, 
      unadjusted=if("negbin" %in% class(nbin_unadj_mod_fit) && nbin_unadj_mod_fit$converged) 
        nbin_unadj_mod_fit
      )  # Set list by predictor adjustment type of fitted Negative Binomial GLMs
    }
  
  
  #####################
  ## Model diagnosis ##
  #####################
  
  diag_mod_ls <- sapply(mod_fit_ls, 
                        FUN=function(mod) 
                          tryCatch(
                            data.frame("Pearson's Chi-squared"=sum(residuals(mod, type="pearson") ^ 2), 
                                       "Residual deviance"=mod$deviance, 
                                       "Residual d.o.f."=mod$df.residual, 
                                       "Pearson's Chi-squared p-value"=pchisq(sum(residuals(mod, type="pearson") ^ 2), 
                                                                              df=mod$df.residual, lower.tail=FALSE
                                                                              ), 
                                       "Residual deviance p-value"=pchisq(mod$deviance, 
                                                                          df=mod$df.residual, lower.tail=FALSE
                                                                          ), 
                                       AIC=mod$aic, 
                                       RMSE=sqrt(mean(residuals(mod, type="response") ^ 2)), 
                                       "Over-dispersion"=summary(mod)$dispersion[1], 
                                       "Over-dispersion p-value"=NA, 
                                       "% observed zeros"=mean(mod$y == 0), 
                                       "% predicted zeros"=if(model_out_fly == "count") 
                                         mean(dpois(0, lambda=mod$fitted.values)) else 
                                           mean(dbinom(0, size=1, prob=mod$fitted.values)), 
                                       t(setNames(unclass(table(design_dat$intervention)), 
                                                  nm=paste(c("Control", "Intervention"), 
                                                           "subgroup size", 
                                                           sep=" "
                                                           )
                                                  )
                                         ),   # Compute and rename sample size by exposure.  N.B.: row-binding required for building single-row table
                                       t(with(design_dat, 
                                              expr=setNames(tapply(get(endpointName), 
                                                                   INDEX=intervention, 
                                                                   FUN=if(model_out_fly == "count") 
                                                                     sum else 
                                                                       function(vec) sum(vec == "1")
                                                                   ), 
                                                            nm=paste(c("Control", "Intervention"), 
                                                                     "event frequency", 
                                                                     sep=" "
                                                                     )
                                                            )
                                              )
                                         ),   # Compute and rename response event counts by exposure.  N.B.: row-binding required for building single-row table
                                       check.names=FALSE
                                       ), 
                            error=function(err) 
                              data.frame("Pearson's Chi-squared"=NA, 
                                         "Residual deviance"=NA, 
                                         "Residual d.o.f."=NA, 
                                         "Pearson's Chi-squared p-value"=NA, 
                                         "Residual deviance p-value"=NA, 
                                         AIC=NA, 
                                         RMSE=NA, 
                                         "Over-dispersion"=NA, 
                                         "Over-dispersion p-value"=NA, 
                                         "% observed zeros"=NA, 
                                         "% predicted zeros"=NA, 
                                         t(setNames(unclass(table(design_dat$intervention)), 
                                                    nm=paste(c("Control", "Intervention"), 
                                                             "subgroup size", 
                                                             sep=" "
                                                             )
                                                    )
                                           ),   # Compute and rename sample size by "intervention".  N.B.: row-binding required for building single-row table
                                         t(with(design_dat, 
                                                expr=setNames(tapply(get(endpointName), 
                                                                     INDEX=intervention, 
                                                                     FUN=if(model_out_fly == "count") 
                                                                       sum else 
                                                                         function(vec) sum(vec == "1")
                                                                     ), 
                                                              nm=paste(c("Control", "Intervention"), 
                                                                       "event frequency", 
                                                                       sep=" "
                                                                       )
                                                              )
                                                )
                                           ),   # Compute and rename response event counts by exposure.  N.B.: row-binding required for building single-row table
                                         check.names=FALSE
                                         )
                            ), 
                        simplify=FALSE
                        )  # Derive list by adjustment type of commonplace diagnostic regression statistics for fitted GLMs
  
  
  if(model_out_fly == "count") 
    diag_nbin_mod_ls <- sapply(nbin_mod_fit_ls, 
                               FUN=function(mod) 
                                 tryCatch(
                                   data.frame("Pearson's Chi-squared"=sum(residuals(mod, type="pearson") ^ 2), 
                                              "Residual deviance"=mod$deviance, 
                                              "Residual d.o.f."=mod$df.residual, 
                                              "Pearson's Chi-squared p-value"=pchisq(sum(residuals(mod, type="pearson") ^ 2), 
                                                                                     df=mod$df.residual, lower.tail=FALSE
                                                                                     ), 
                                              "Residual deviance p-value"=pchisq(mod$deviance, 
                                                                                 df=mod$df.residual, lower.tail=FALSE
                                                                                 ), 
                                              AIC=mod$aic, 
                                              RMSE=sqrt(mean(residuals(mod, type="response") ^ 2)), 
                                              "Over-dispersion"=1 / mod$theta, 
                                              "Over-dispersion p-value"=.5 * pchisq(2 * (logLik(mod) - logLik(mod_fit_ls$adjusted)), 
                                                                                    df=1, lower.tail=FALSE
                                                                                    ), 
                                              "% observed zeros"=mean(mod$y == 0), 
                                              "% predicted zeros"=mean(dnbinom(0, 
                                                                               size=mod$theta, 
                                                                               mu=mod$fitted.values
                                                                               )
                                                                       ), 
                                              t(setNames(unclass(table(design_dat$intervention)), 
                                                         nm=paste(c("Control", "Intervention"), 
                                                                  "subgroup size", 
                                                                  sep=" "
                                                                  )
                                                         )
                                                ),   # Compute and rename sample size by exposure.  N.B.: row-binding required for building single-row table
                                              t(with(design_dat, 
                                                     expr=setNames(tapply(get(endpointName), 
                                                                          INDEX=intervention, 
                                                                          FUN=sum
                                                                          ), 
                                                                   nm=paste(c("Control", "Intervention"), 
                                                                            "event frequency", 
                                                                            sep=" "
                                                                            )
                                                                   )
                                                     )
                                                ),   # Compute and rename response event counts by exposure.  N.B.: row-binding required for building single-row table
                                              check.names=FALSE
                                              ), 
                                   error=function(err) 
                                     data.frame("Pearson's Chi-squared"=NA, 
                                                "Residual deviance"=NA, 
                                                "Residual d.o.f."=NA, 
                                                "Pearson's Chi-squared p-value"=NA, 
                                                "Residual deviance p-value"=NA, 
                                                AIC=NA, 
                                                RMSE=NA, 
                                                "Over-dispersion"=NA, 
                                                "Over-dispersion p-value"=NA, 
                                                "% observed zeros"=NA, 
                                                "% predicted zeros"=NA, 
                                                t(setNames(unclass(table(design_dat$intervention)), 
                                                           nm=paste(c("Control", "Intervention"), 
                                                                    "subgroup size", 
                                                                    sep=" "
                                                                    )
                                                           )
                                                  ),   # Compute and rename sample size by "intervention".  N.B.: row-binding required for building single-row table
                                                t(with(design_dat, 
                                                       expr=setNames(tapply(get(endpointName), 
                                                                            INDEX=intervention, 
                                                                            FUN=sum
                                                                            ), 
                                                                     nm=paste(c("Control", "Intervention"), 
                                                                              "event frequency", 
                                                                              sep=" "
                                                                              )
                                                                     )
                                                       )
                                                  ),   # Compute and rename response event counts by exposure.  N.B.: row-binding required for building single-row table
                                                check.names=FALSE
                                                )
                                   ), 
                               simplify=FALSE
                               )  # Derive list by adjustment type of commonplace diagnostic regression statistics for fitted Negative Binomial GLMs
  
  
  #######################
  ## Output tabulation ##
  #######################
  
  inf_out_ls <- sapply(mod_fit_ls, 
                       FUN=function(mod) 
                         tryCatch(coeftest(mod, vcov.=vcovCL, cluster=~ subclass), 
                                  error=function(err) 
                                    array(NA, 
                                          dim=c(length(mod$coefficients), 4), 
                                          dimnames=list(names(mod$coefficients), 
                                                        c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
                                                        )
                                          )
                                  ), 
                       simplify=FALSE
                       )  # Derive list by adjustment type of cluster-adjusted inferences from fitted GLMs
  
  
  mod_out_ls <- sapply(names(inf_out_ls), 
                       FUN=function(nm) 
                         tryCatch(cbind(
                           data.frame(Analysis=analysisLabel, 
                                      Subgroup=subgroupLabel, 
                                      Model=unname(model_out_fly), 
                                      Type=nm, 
                                      Endpoint=endpointName, 
                                      Predictors=rep(paste(rownames(inf_out_ls[[nm]][-1, , drop=FALSE]), 
                                                           paste0("(estimate = ", 
                                                                  round(exp(inf_out_ls[[nm]][-1, "Estimate", drop=FALSE]), 
                                                                        digits=2
                                                                        ), 
                                                                  ", p-value = ", 
                                                                  round(inf_out_ls[[nm]][-1, "Pr(>|z|)", drop=FALSE], 
                                                                        digits=3
                                                                        ), 
                                                                  ")"
                                                                  ), 
                                                           collapse=";\n"
                                                           ), 
                                                     times=length(grep("intervention", 
                                                                       x=rownames(inf_out_ls[[nm]]), 
                                                                       value=TRUE
                                                                       )
                                                                  ) + 
                                                       ifelse(is.null(interactionName), 0, 1)
                                                     ), 
                                      Coefficient=c(grep("intervention", 
                                                         x=names(mod_fit_ls[[nm]]$coefficients), 
                                                         value=TRUE
                                                         ), 
                                                    if(! is.null(interactionName)) 
                                                      paste(grep("intervention", 
                                                                 x=names(mod_fit_ls[[nm]]$coefficients), 
                                                                 value=TRUE
                                                                 ), 
                                                            collapse=" + "
                                                            )
                                                    ), 
                                      "Rate ratio"=exp(c(mod_fit_ls[[nm]]$coefficients[grep("intervention", 
                                                                                            x=names(mod_fit_ls[[nm]]$coefficients), 
                                                                                            value=TRUE
                                                                                            )], 
                                                            if(! is.null(interactionName)) 
                                                              coef(glht(mod_fit_ls[[nm]], 
                                                                        linfct=t(ifelse(grepl("intervention", 
                                                                                              x=names(mod_fit_ls[[nm]]$coefficients)
                                                                                              ), 
                                                                                        1, 0
                                                                                        )
                                                                                 ), 
                                                                        vcov.=vcovCL, cluster= ~ subclass
                                                                        )
                                                                   )
                                                            )
                                                          ), 
                                      rbind(exp(rbind(coefci(mod_fit_ls[[nm]], 
                                                             parm=grep("intervention", 
                                                                       x=names(mod_fit_ls[[nm]]$coefficients), 
                                                                       value=TRUE
                                                                       ), 
                                                             level=.95, vcov.=vcovCL, cluster= ~ subclass
                                                             ), 
                                                      if(! is.null(interactionName)) 
                                                        confint(glht(mod_fit_ls[[nm]], 
                                                                     linfct=t(ifelse(grepl("intervention", 
                                                                                           x=names(mod_fit_ls[[nm]]$coefficients)
                                                                                           ), 
                                                                                     1, 0
                                                                                     )
                                                                              ), 
                                                                     vcov.=vcovCL, cluster= ~ subclass
                                                                     ), 
                                                                level=.95
                                                                )$confint[, c("lwr", "upr")]
                                                      )
                                                )
                                            ),   # N.B.: row-binding required for building single-row table
                                      "P-value"=c(inf_out_ls[[nm]][grep("intervention", 
                                                                        x=names(mod_fit_ls[[nm]]$coefficients), 
                                                                        value=TRUE
                                                                        ), 
                                                                   "Pr(>|z|)"], 
                                                  if(! is.null(interactionName)) 
                                                    summary(glht(mod_fit_ls[[nm]], 
                                                                 linfct=t(ifelse(grepl("intervention", 
                                                                                       x=names(mod_fit_ls[[nm]]$coefficients)
                                                                                       ), 
                                                                                 1, 0
                                                                                 )
                                                                          ), 
                                                                 vcov.=vcovCL, cluster= ~ subclass
                                                                 )
                                                            )$test$pvalues[1]
                                                  ), 
                                      check.names=FALSE
                                      ), 
                           diag_mod_ls[[nm]][rep.int(1, 
                                                     times=length(grep("intervention", 
                                                                       x=names(mod_fit_ls[[nm]]$coefficients), 
                                                                       value=TRUE
                                                                       )
                                                                  ) + 
                                                       ifelse(is.null(interactionName), 0, 1)
                                                     ), ]
                           ),   # Set data-frame of key inferences from Poisson regression models
                           error=function(err) 
                             cbind(
                               data.frame(Analysis=analysisLabel, 
                                          Subgroup=subgroupLabel, 
                                          Model=unname(model_out_fly), 
                                          Type=nm, 
                                          Endpoint=endpointName, 
                                          Predictors=paste(predictorNames_vec, 
                                                           collapse=";\n"
                                                           ), 
                                          Coefficient="intervention", 
                                          "Rate ratio"=NA, 
                                          "2.5 %"=NA, 
                                          "97.5 %"=NA, 
                                          "P-value"=NA, 
                                          check.names=FALSE
                                          ), 
                               diag_mod_ls[[nm]]
                               )
                           ), 
                       simplify=FALSE
                       )  # Derive list by adjustment type of inferences for fitted GLMs
  
  mod_out_dat <- do.call(rbind, 
                         args=mod_out_ls
                         )  # Merge inferences from GLM regressions into single data-frame
  
  
  if(model_out_fly == "count"){
    nbin_inf_out_ls <- sapply(nbin_mod_fit_ls, 
                         FUN=function(mod) 
                           tryCatch(coeftest(mod, vcov.=vcovCL, cluster=~ subclass), 
                                    error=function(err) 
                                      array(NA, 
                                            dim=c(length(mod$coefficients), 4), 
                                            dimnames=list(names(mod$coefficients), 
                                                          c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
                                                          )
                                            )
                                    ), 
                         simplify=FALSE
                         )  # Derive list by adjustment type of cluster-adjusted inferences from fitted Negative Binomial GLMs
    
    
    nbin_mod_out_ls <- sapply(names(nbin_inf_out_ls), 
                              FUN=function(nm) 
                                tryCatch(cbind(
                                  data.frame(Analysis=analysisLabel, 
                                             Subgroup=subgroupLabel, 
                                             Model="nbin", 
                                             Type=nm, 
                                             Endpoint=endpointName, 
                                             Predictors=rep(paste(rownames(nbin_inf_out_ls[[nm]][-1, , drop=FALSE]), 
                                                                  paste0("(estimate = ", 
                                                                         round(exp(nbin_inf_out_ls[[nm]][-1, "Estimate"]), 
                                                                               digits=2
                                                                               ), 
                                                                         ", p-value = ", 
                                                                         round(nbin_inf_out_ls[[nm]][-1, "Pr(>|z|)"], 
                                                                               digits=3
                                                                               ), 
                                                                         ")"
                                                                         ), 
                                                                  collapse=";\n"
                                                                  ), 
                                                            times=length(grep("intervention", 
                                                                              x=rownames(nbin_inf_out_ls[[nm]]), 
                                                                              value=TRUE
                                                                              )
                                                                         ) + 
                                                              ifelse(is.null(interactionName), 0, 1)
                                                            ), 
                                             Coefficient=c(grep("intervention", 
                                                                x=rownames(nbin_inf_out_ls[[nm]]), 
                                                                value=TRUE
                                                                ), 
                                                           if(! is.null(interactionName)) 
                                                             paste(grep("intervention", 
                                                                        x=rownames(nbin_inf_out_ls[[nm]]), 
                                                                        value=TRUE
                                                                        ), 
                                                                   collapse=" + "
                                                                   )
                                                           ), 
                                             "Rate ratio"=exp(c(nbin_inf_out_ls[[nm]][grep("intervention", 
                                                                                              x=rownames(nbin_inf_out_ls[[nm]]), 
                                                                                              value=TRUE
                                                                                              ), "Estimate"], 
                                                                   if(! is.null(interactionName)) 
                                                                     coef(glht(nbin_mod_fit_ls[[nm]], 
                                                                               linfct=t(ifelse(grepl("intervention", 
                                                                                                     x=rownames(nbin_inf_out_ls[[nm]])
                                                                                                     ), 
                                                                                               1, 0
                                                                                               )
                                                                                        ), 
                                                                               vcov.=vcovCL, cluster= ~ subclass
                                                                               )
                                                                          )
                                                                   )
                                                                 ), 
                                             rbind(exp(rbind(coefci(nbin_mod_fit_ls[[nm]], 
                                                                    parm=grep("intervention", 
                                                                              x=rownames(nbin_inf_out_ls[[nm]]), 
                                                                              value=TRUE
                                                                              ), 
                                                                    level=.95, vcov.=vcovCL, cluster= ~ subclass
                                                                    ), 
                                                             if(! is.null(interactionName)) 
                                                               confint(glht(mod_fit_ls[[nm]], 
                                                                            linfct=t(ifelse(grepl("intervention", 
                                                                                                  x=rownames(nbin_inf_out_ls[[nm]])
                                                                                                  ), 
                                                                                            1, 0
                                                                                            )
                                                                                     ), 
                                                                            vcov.=vcovCL, cluster= ~ subclass
                                                                            ), 
                                                                       level=.95
                                                                       )$confint[, c("lwr", "upr")]
                                                             )
                                                       )
                                                   ),   # N.B.: row-binding required for building single-row table
                                             "P-value"=c(nbin_inf_out_ls[[nm]][grep("intervention", 
                                                                                    x=rownames(nbin_inf_out_ls[[nm]]), 
                                                                                    value=TRUE
                                                                                    ), 
                                                                               "Pr(>|z|)"], 
                                                         if(! is.null(interactionName)) 
                                                           summary(glht(nbin_mod_fit_ls[[nm]], 
                                                                        linfct=t(ifelse(grepl("intervention", 
                                                                                              x=rownames(nbin_inf_out_ls[[nm]])
                                                                                              ), 
                                                                                        1, 0
                                                                                        )
                                                                                 ), 
                                                                        vcov.=vcovCL, cluster= ~ subclass
                                                                        )
                                                                   )$test$pvalues[1]
                                                         ), 
                                             check.names=FALSE
                                             ), 
                                  diag_nbin_mod_ls[[nm]][rep.int(1, times=length(grep("intervention", 
                                                                                      x=rownames(nbin_inf_out_ls[[nm]]), 
                                                                                      value=TRUE
                                                                                      )
                                                                                 ) + 
                                                                   ifelse(is.null(interactionName), 0, 1)
                                                                 ), ]
                                  ),   # Set data-frame of key inferences from Poisson regression models
                                  error=function(err) 
                                    cbind(
                                      data.frame(Analysis=analysisLabel, 
                                                 Subgroup=subgroupLabel, 
                                                 Model="nbin", 
                                                 Type=nm, 
                                                 Endpoint=endpointName, 
                                                 Predictors=paste(predictorNames_vec, 
                                                                  collapse=";\n"
                                                                  ), 
                                                 Coefficient="intervention", 
                                                 "Rate ratio"=NA, 
                                                 "2.5 %"=NA, 
                                                 "97.5 %"=NA, 
                                                 "P-value"=NA, 
                                                 check.names=FALSE
                                                 ), 
                                      diag_nbin_mod_ls[[nm]]
                                      )
                                  ), 
                              simplify=FALSE
                              )  # Derive list by adjustment type of inferences for fitted Negative Binomial GLMs
    
    nbin_mod_out_dat <- do.call(rbind, 
                                args=nbin_mod_out_ls
                                )  # Merge inferences from Negative Binomial GLM regressions into single data-frame
    
    
    mod_out_dat <- rbind(mod_out_dat, nbin_mod_out_dat)  # Stack data-frames of GLM regression inferences
    } else 
      names(mod_out_dat)[names(mod_out_dat) == "Rate ratio"] <- "Odds ratio"  # Rename inferences data-frame variable
  
  
  #####################################
  # Export fitted regression models  ##
  # by analysis data-set and outcome ##
  #####################################
  
  endpointName <- sub("(^.+)(_end$)", 
                      replacement="\\1", 
                      x=endpointName
                      )  # Rename the response variable
  
  
  write.table(mod_out_dat, 
              file=file.path(outputPath, 
                             paste0(paste(endpointName, "glm", subgroupLabel, 
                                          ifelse(is.character(interactionName), 
                                                 paste("int", 
                                                       paste(interactionLabel, collapse="."), 
                                                       sep="."
                                                       ), 
                                                 "noint"
                                                 ), 
                                          sep="_"
                                          ), 
                                    "_", analysisLabel, ".csv"
                                    )
                             ), 
              quote=TRUE, sep=",", row.names=FALSE, col.names=TRUE
              )  # Export  in .csv format inferences data-frame by outcome, model and subgroup
  
  return(list(endpointName=endpointName, 
              predictorNames=predictorNames_vec, 
              analysisLabel=analysisLabel, 
              subgroupLabel=subgroupLabel, 
              interactionNames=interactionName
              )
         )  # Return as output a list of models' terms
  }