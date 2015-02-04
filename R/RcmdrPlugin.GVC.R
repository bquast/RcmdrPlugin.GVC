# loads Rcmdr library upon being loaded
# copied for J. Fox's package: RcmdrPlugin.TeachingDemos (version 1.0-7)
.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())    
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}

#' @export
#' @import Rcmdr decompr

gvc_decomp <- function(){
  require(decompr)
  
  defaults <- list (initial.x = NULL, initial.alternative = "two.sided", initial.level = ".95", 
                    initial.mu = "0.0")
  dialog.values <- getDialog ("singleSampleTTest", defaults)  
  initializeDialog(title = gettextRcmdr("Single-Sample t-Test"))
  xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),
                          initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  onOK <- function() {
    x <- getSelection(xBox)
    if (length(x) == 0) {
      errorCondition(recall = singleSampleTTest, message = gettextRcmdr("You must select a variable."))
      return()
    }
    alternative <- as.character(tclvalue(alternativeVariable))
    level <- tclvalue(confidenceLevel)
    mu <- tclvalue(muVariable)
    putDialog ("singleSampleTTest", list (initial.x = x, initial.alternative = alternative, 
                                          initial.level = level, initial.mu = mu))
    closeDialog()
    doItAndPrint(paste("with(", ActiveDataSet (), ", (t.test(", x, 
                       ", alternative='", alternative, "', mu=", mu, ", conf.level=", 
                       level, ")))", sep = ""))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "decomp", reset = "gvc_decomp")
  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided", 
                                                               "less", "greater"), values = c("two.sided", "less", "greater"), 
               labels = gettextRcmdr(c("Population mean != mu0", "Population mean < mu0", 
                                       "Population mean > mu0")), title = gettextRcmdr("Alternative Hypothesis"),
               initialValue = dialog.values$initial.alternative)
  rightFrame <- tkframe(optionsFrame)
  confidenceFrame <- tkframe(rightFrame)
  confidenceLevel <- tclVar(dialog.values$initial.level)
  confidenceField <- ttkentry(confidenceFrame, width = "6", 
                              textvariable = confidenceLevel)
  muFrame <- tkframe(rightFrame)
  muVariable <- tclVar(dialog.values$initial.mu)
  muField <- ttkentry(muFrame, width = "8", textvariable = muVariable)
  tkgrid(getFrame(xBox), sticky = "nw")
  tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
  tkgrid(labelRcmdr(muFrame, text = gettextRcmdr("Null hypothesis: mu = ")), 
         muField, sticky = "w", padx=c(10, 0))
  tkgrid(muFrame, sticky = "w")
  tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level: ")), 
         confidenceField, sticky = "w", padx=c(10, 0))
  tkgrid(confidenceFrame, sticky = "w")
  tkgrid(alternativeFrame, rightFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  tkgrid.configure(confidenceField, sticky = "e")
  dialogSuffix()
  

}