# Forest plot function                                                      
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


forest_plot_SC <- function(DATA, period) {
  
  
  SMD.dat <- DATA %>%
    as.data.frame() %>%
    rename('Full sample' = original, `Matched sample` = matched)
  
  
  SMD.arr <- data.matrix(SMD.dat[! grepl("^ccg", x=rownames(SMD.dat)), ])  # Subset SMD data-frame to non-CCG rows and format to 2d-array
  
  names(dimnames(SMD.arr)) <- c("Variable", "Type")  # Set SMD 2d-array margin names
  
  bal_cutoff <- .3  # Set SMD balance cut-off
  
  # Forest plots 
  # of SMD statistics
  
  SMD_trim.arr <- sign(SMD.arr) * 
    pmin(abs(SMD.arr), bal_cutoff)  # Derive 2d-array of trimmed SMD statistics
  
  
  SMD_trim.ls <- list(Left=SMD_trim.arr[seq_len(nrow(SMD_trim.arr) %/% 2), ], 
                      Right=SMD_trim.arr[- seq_len(nrow(SMD_trim.arr) %/% 2), ]
  )  # Set list of split 2d-arrays by "Variable", "Type" of selected SMD statistics
  
  
  par(mfrow=seq.int(2),   # Split graphical display in 1x2 panes
      mar=c(5, 4 + 3.5, 4, 2) + .1  # Move left plot margin inwards
  )
  
  plot(SMD_trim.ls[["Left"]], 
       row(SMD_trim.ls[["Left"]]), 
       type="p", 
       col=matrix(rainbow(ncol(SMD_trim.ls[["Left"]])), 
                  nrow=nrow(SMD_trim.ls[["Left"]]), 
                  ncol=ncol(SMD_trim.ls[["Left"]]), 
                  byrow=TRUE), 
       pch=ifelse(abs(SMD_trim.ls[["Left"]]) < bal_cutoff, 
                  matrix(14 + seq.int(ncol(SMD_trim.ls[["Left"]])), 
                         nrow=nrow(SMD_trim.ls[["Left"]]), 
                         ncol=ncol(SMD_trim.ls[["Left"]]), 
                         byrow=TRUE), 
                  matrix(-1 + seq.int(ncol(SMD_trim.ls[["Left"]])), 
                         nrow=nrow(SMD_trim.ls[["Left"]]), 
                         ncol=ncol(SMD_trim.ls[["Left"]]), 
                         byrow=TRUE)
       ), 
       axes=FALSE, 
       xlab="SMD (%)", ylab="", 
       xlim=c(-1, 1) * bal_cutoff
  )  # Plot forest plot of SMD statistics
  
  axis(1, 
       at=sprintf("%.1f", 
                  seq(-bal_cutoff, bal_cutoff, by=.1)
       ), 
       labels=sprintf("%2.0f", 
                      1e2 * seq(-bal_cutoff, bal_cutoff, by=.1)
       ), 
       cex.axis=.7
  )  # Overlay x-axis labels onto forest plot
  
  axis(2, 
       at=seq_len(nrow(SMD_trim.ls[["Left"]])), 
       labels=rownames(SMD_trim.ls[["Left"]]), 
       cex.axis=.7, las=1
  )  # Overlay y-axis labels onto forest plot
  
  abline(h=seq_len(nrow(SMD_trim.ls[["Left"]])),
         col=8, lty=3
  )  # Overlay horizontal lines across variables for readability
  
  abline(v=seq(-.1, .1, by=.1), 
         col=8, lty=c(2, 4, 2)
  )  # Overlay vertical lines across SMD thresholds for readability
  
  legend("topleft", 
         legend=dimnames(SMD_trim.ls[["Left"]])$Type, 
         col=rainbow(ncol(SMD_trim.ls[["Left"]]
         )
         ), 
         pch=15:17, 
         bg="transparent", cex=.7, bty="n"
  )  # Overlay legend with "Type" keys
  
  legend("bottomleft", 
         legend=c("Actual", "Trimmed"), 
         pch=c(15, 0), 
         bg="transparent", cex=.7, bty="n"
  )  # Overlay legend with "Trimming" keys
  
  plot(SMD_trim.ls[["Right"]], 
       row(SMD_trim.ls[["Right"]]), 
       type="p", 
       col=matrix(rainbow(ncol(SMD_trim.ls[["Right"]])), 
                  nrow=nrow(SMD_trim.ls[["Right"]]), 
                  ncol=ncol(SMD_trim.ls[["Right"]]), 
                  byrow=TRUE
       ), 
       pch=ifelse(abs(SMD_trim.ls[["Right"]]) < bal_cutoff, 
                  matrix(14 + seq.int(ncol(SMD_trim.ls[["Right"]])), 
                         nrow=nrow(SMD_trim.ls[["Right"]]), 
                         ncol=ncol(SMD_trim.ls[["Right"]]), 
                         byrow=TRUE
                  ), 
                  matrix(-1 + seq.int(ncol(SMD_trim.ls[["Right"]])), 
                         nrow=nrow(SMD_trim.ls[["Right"]]), 
                         ncol=ncol(SMD_trim.ls[["Right"]]), 
                         byrow=TRUE
                  )
       ), 
       axes=FALSE, 
       xlab="SMD (%)", ylab="", 
       xlim=c(-1, 1) * bal_cutoff
  )  # Plot forest plot of SMD statistics
  
  axis(1, 
       at=sprintf("%.1f", 
                  seq(-bal_cutoff, bal_cutoff, by=.1)
       ), 
       labels=sprintf("%2.0f", 
                      1e2 * seq(-bal_cutoff, bal_cutoff, by=.1)
       ), 
       cex.axis=.7
  )  # Overlay x-axis labels onto forest plot
  
  axis(2, 
       at=seq_len(nrow(SMD_trim.ls[["Right"]])), 
       labels=rownames(SMD_trim.ls[["Right"]]), 
       cex.axis=.7, las=1
  )  # Overlay y-axis labels onto forest plot
  
  abline(h=seq_len(nrow(SMD_trim.ls[["Right"]])),
         col=8, lty=3
  )  # Overlay horizontal lines across variables for readability
  
  abline(v=seq(-.1, .1, by=.1), 
         col=8, lty=c(2, 4, 2)
  )  # Overlay vertical lines across SMD thresholds for readability
  
  mtext(paste("Standardised Mean Differences", 
              # gsub(" ([[:alpha:]])", 
              #      replacement=" \\U\\1", 
              #      x=names(sc_om), 
              #      perl=TRUE), 
              sep=" "
  ), 
  outer=TRUE, side=3, 
  cex=1.5, font=2, line=-3.25
  )  # Overlay main title
  
  mtext("Nearest Neighbour matching", 
        outer=TRUE, side=1, 
        cex=.75, font=1, line=-1.75
  )  # Overlay subtitle
  
  dev.print(png, 
            file=paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/Output/19_April/SMD_exact_", period, control, "_mth.png", sep = ''), 
            width=1024, height=768, units="px"
  )  # Export in .png format SMD forest plots
  
  
  
}

period = "24"
