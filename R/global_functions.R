# global helper function
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Lapply with error
# to let things run through even if there is an error
lapply_with_error <- function(X,FUN,...){    
  lapply(X, function(x, ...) tryCatch(FUN(x, ...),
                                      error=function(e) NULL))
}


# Summarize brms-fit objects
# function copied from here: https://github.com/m-clark/lazerhawk/blob/master/R/brms_SummaryTable.R#L106
brms_SummaryTable <- function(model,
                              formatOptions=list(digits=2, nsmall=2),
                              round=2,
                              astrology=FALSE,
                              hype=FALSE,
                              panderize=FALSE,
                              justify=NULL,
                              seed = 1234,
                              ...) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("brms package, along with related dependencies, is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  if (!inherits(model, "brmsfit")) stop('Model is not a brmsfit class object.')
  
  fe = brms::fixef(model)
  partables_formatted = data.frame(Covariate=rownames(fe),
                                   do.call(format, list(x=round(fe, round), unlist(formatOptions))))
  rownames(partables_formatted) = NULL
  colnames(partables_formatted)[4:5] = c('l-95% CI', 'u-95% CI')
  
  if(!astrology & !hype & !panderize) {
    return(partables_formatted)
  }
  
  # conduct hypothesis tests
  if (hype) {
    testnams = mapply(function(coefname, sign)
      ifelse(sign>0, paste0(coefname, ' > 0'), paste0(coefname, ' < 0')), rownames(fe), sign(fe[,'Estimate']))
    hypetests = brms::hypothesis(x=model, testnams, seed=seed)
    ER = hypetests$hypothesis$Evid.Ratio
    partables_formatted$pvals = ER/(ER+1)
    partables_formatted$pvals[is.infinite(ER)] = 1
    partables_formatted$pvals = do.call(format, list(x=round(partables_formatted$pvals, round), formatOptions[[1]]))
    colnames(partables_formatted)[ncol(partables_formatted)] = 'B < > 0'
    partables_formatted[,'Evidence Ratio'] = do.call(format, list(x=round(ER, round), formatOptions[[1]]))
  }
  
  # if hype, make star based on brms result, else interval
  if (astrology && hype)  {
    partables_formatted$Notable = hypetests$hypothesis$Star
  } else if (astrology) {
    partables_formatted$Notable =  apply(sign(fe[,c('Q2.5', 'Q97.5')]),
                                         1,
                                         function(interval) ifelse(diff(interval)==0, '*', ''))
  }
  
  if (panderize) {
    if (is.null(justify)) {
      if (astrology) {
        justify = paste0('l', paste0(rep('r', ncol(partables_formatted)-2), collapse = ''), 'c')
      } else {
        justify = paste0('l', paste0(rep('r', ncol(partables_formatted)-1), collapse = ''))
      }
    }
    return(pander::pander(x=partables_formatted, justify=justify, split.tables=Inf))
  }
  
  partables_formatted
}
