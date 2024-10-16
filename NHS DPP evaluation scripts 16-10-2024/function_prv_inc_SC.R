################################
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

################################
## Estimate follow-up         ##
## person-years, prevalence,  ##
## incidence in evaluations   ##


prv.inc_iau_fn <- function(endpointName, 
                           strataNames_vec="intervention", 
                           design, 
                           analysisLabel=NULL, 
                           subgroupLabel=NULL, 
                           strataLabel=NULL, 
                           outputPath=NULL
                           )
  {
  # Function computes incidence (for count endpoints), prevalence (for binary endpoints)
  # and follow-up person-time from an analysis following IAU data dictionary conventions
  # and exports formatted tabulating data-frame.
  #
  #
  # Arguments:
  #
  # endpointName    : character string denoting the response variable's name
  # strataNames_vec : character vector denoting the name of stratifying variables
  # design          : design data-frame or weighted survey design object
  # analysisLabel   : character string denoting the analysis type label
  # subgroupLabel   : character string denoting the subgroup analysis label
  # strataLabel     : character string denoting the stratification label
  # outputPath      : character string denoting the path to the output destination
  #                   folder
  #
  #
  # Output:
  #
  # Data-frame of prevalence, incidence and follow-up person-time statistics (rows) 
  # stratified by subgroups (columns) produced as a .csv table (if 'outputPath' 
  # is specified).
  
  
  ##############
  ## Preamble ##
  ##############

  require(survey)  # Load library required for survey sampling
  
  
  stopifnot(is.character(endpointName),
            any(is.data.frame(design) | class(design) == "survey.design"), 
            is.null(strataNames_vec) | is.character(strataNames_vec), 
            is.null(analysisLabel) | is.character(analysisLabel),
            is.null(subgroupLabel) | is.character(subgroupLabel),
            is.null(strataLabel) | is.character(strataLabel)
            )
  
  if(! any(c(endpointName, strataNames_vec) %in%
           if(is.data.frame(design))
             names(design) else
               names(design$variables)
           )
     ) 
    stop("At least one of the endpoint or stratifying variables is not in the analysis data-set.")
  
  if(is.character(outputPath)) 
     if(! dir.exists(outputPath)) 
       dir.create(outputPath, recursive=TRUE)
  
  
  #########################
  ## Data pre-processing ##
  #########################
  
  if(is.data.frame(design)){
    design[strataNames_vec] <- lapply(design[strataNames_vec], 
                                      FUN=factor
                                      )  # Format binary variables from analysis data-frame as factors
    
    endpointType <- ifelse(length(unique(design[[endpointName]])) == 2, 
                           "binary", "count"
                           )  # Set endpoint type from analysis data-frame
    } else {
      design$variables[strataNames_vec] <- lapply(design$variables[strataNames_vec], 
                                                  FUN=factor
                                                  )  # Format binary variables from weighted analysis data-frame as factors
      
      endpointType <- ifelse(length(unique(design$variables[[endpointName]])) == 2, 
                             "binary", "count"
                             )  # Set endpoint type from weighted analysis data-frame
      }
  
  
  str_frm <- as.formula(paste("", 
                              paste(strataNames_vec, 
                                    collapse=" + "
                                    ), 
                              sep=" ~ "
                              )
                        )  # Set formula for cross-tabulation by stratifying variables
  
  
  ##############################
  ## Derive target statistics ##
  ##############################
  
  if(is.data.frame(design))
    {
    pid_vec <- aggregate(update(str_frm, new=pid ~ .), 
                         data=design, 
                         FUN=function(vec) length(unique(vec))
                         )[, length(strataNames_vec) + 1]  # Derive vector of "pid" frequencies by stratifying variables from analysis data-frame
    
    
    uid_vec <- xtabs(str_frm, 
                     data=design
                     )  # Derive 2d-array of "uid" frequencies by stratifying variables from analysis data-frame
    
    
    py_vec <- aggregate(update(str_frm, new=studylength ~ .), 
                        data=design, 
                        FUN=sum)[, "studylength"] / 365.25   # Derive vector of follow-up person-years by stratifying variables from analysis data-frame
    
    
    freq_vec <- aggregate(update(str_frm, new=I(get(endpointName)) ~ .), 
                          data=design, 
                          FUN=sum
                          )[, length(strataNames_vec) + 1]  # Derive vector of endpoint frequencies by stratifying variables from analysis data-frame  
    
    
    prv.inc_vec <- if(endpointType == "binary") 
      aggregate(update(str_frm, new=I(get(endpointName) > 0) ~ .), 
                data=design, 
                FUN=mean
                )[, length(strataNames_vec) + 1] else 
                  freq_vec / py_vec   # Derive vector of prevalences (for binary endpoint) or incidences (for count endpoint) by stratifying variables from analysis data-frame
    } else {
      pid_vec <- svytable(str_frm, 
                          design=design
                          )  # Derive 2d-array of "pid" frequencies by stratifying variables from weighed analysis data-frame
      
      
      uid_vec <- xtabs(str_frm, 
                       data=design$variables
                       )  # Derive 2d-array of "uid" frequencies by stratifying variables from weighed analysis data-frame
      
      
      py_vec <- svyby(~ studylength, 
                      by=str_frm, 
                      design=design, 
                      FUN=svytotal
                      )[, "studylength"] / 365.25  # Derive vector of follow-up person-years by stratifying variables from weighed analysis data-frame
      
      
      freq_vec <- svyby(~ I(get(endpointName)), 
                        by=str_frm, 
                        design=design, 
                        FUN=svytotal
                        )[, length(strataNames_vec) + 1]  # Derive vector of endpoint frequencies by stratifying variables from weighed analysis data-frame
      
      
      prv.inc_vec <- if(endpointType == "binary") 
        svyby(~ I(get(endpointName) > 0), 
              by=str_frm, 
              design=design, 
              FUN=svymean
              )[, length(strataNames_vec) + 2] else 
                freq_vec / py_vec  # Derive vector of prevalences (for binary endpoint) or incidence (for count endpoint) by stratifying variables from weighed analysis data-frame
      }
  
  
  ################################
  ## Tabulate target statistics ##
  ################################
  
  end_dat <- data.frame("No. people"=format(round(c(pid_vec)), trim=TRUE, scientific=FALSE, big.mark=","), 
                        "No. unique records"=format(round(c(uid_vec)), trim=TRUE, scientific=FALSE, big.mark=","), 
                        "Follow-up person-years"=format(py_vec, trim=TRUE, digits=1, scientific=FALSE, big.mark=","), 
                        Frequency=format(round(freq_vec), trim=TRUE,  scientific=FALSE, big.mark=","), 
                        prv.inc=if(endpointType == "binary") 
                          sprintf("%.2f%%", 1e2 * prv.inc_vec) else 
                            sprintf("%.2f", prv.inc_vec), 
                        row.names=apply(expand.grid(dimnames(uid_vec)), 
                                        MARGIN=1, 
                                        FUN=function(vec) 
                                          paste(strataNames_vec, vec, 
                                                collapse=", ", sep=" = "
                                                )
                                        ), 
                        check.names=FALSE
                        )  # Set data-frame of target statistics
  
  names(end_dat)[names(end_dat) %in% "prv.inc"] <- ifelse(endpointType == "binary", 
                                                          "Prevalence", "Incidence"
                                                          )  # Rename target statistics data-frame prevalence / incidence variable
  
  
  end_dat <- t(end_dat)  # Transpose target statistics data-frame
  
  
  ####################
  ## Export tables  ##
  ## to file system ##
  ####################
  
  endpointLabel <- sub("_end", replacement="", x=endpointName
                       )  # Derive endpoint label
  
  if(is.character(outputPath)) 
    write.table(end_dat, 
                file=file.path(outputPath, 
                               paste0(paste(endpointLabel, strataLabel, subgroupLabel, 
                                            analysisLabel, 
                                            sep="_"
                                            ), 
                                      ".csv"
                                      )
                               ), 
                quote=TRUE, sep=",", row.names=TRUE, col.names=NA
                )  # Export in .csv format by type baseline statistics 2d-arrays
  
  
  return(end_dat)  # Return as output target statistics data-frame
  }