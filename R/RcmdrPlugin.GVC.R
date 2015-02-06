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
  
  dataSets <- listDataSets()
  defaults <- list (initial.x = NULL, initial.y = NULL, initial.alternative = "two.sided", initial.level = ".95", 
                    initial.mu = "0.0")
  dialog.values <- getDialog ("gvc_decomp", defaults)  
  initializeDialog(title = gettextRcmdr("GVC Decomposition"))
  xBox <- variableListBox(top, dataSets, title = gettextRcmdr("Intermediate Demand") )
  yBox <- variableListBox(top, dataSets, title = gettextRcmdr("Final Demand") )
  kBox <- variableListBox(top, dataSets, title = gettextRcmdr("Countries / Regions") )
  iBox <- variableListBox(top, dataSets, title = gettextRcmdr("Industries") )
  oBox <- variableListBox(top, dataSets, title = gettextRcmdr("Final Output") )
  onOK <- function() {
    x <- getSelection(xBox)
    y <- getSelection(yBox)
    k <- getSelection(kBox)
    i <- getSelection(iBox)
    o <- getSelection(oBox)
    if (length(x) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for x."))
      return()
    }
    if (length(y) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for y."))
      return()
    }
    if (length(k) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for k."))
      return()
    }
    if (length(i) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for i."))
      return()
    }
    if (length(o) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for o."))
      return()
    }
    alternative <- as.character(tclvalue(alternativeVariable))
    level <- tclvalue(confidenceLevel)
    mu <- tclvalue(muVariable)
    putDialog ("gvc_decomp", list (initial.x = x, initial.alternative = alternative, 
                                          initial.level = level, initial.mu = mu))
    closeDialog()
    doItAndPrint(paste("with(", ActiveDataSet (), ", (t.test(", x, 
                       ", alternative='", alternative, "'", sep = ""))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "decomp", reset = "gvc_decomp")
  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided", 
                                                               "less", "greater"), values = c("two.sided", "less", "greater"), 
               labels = gettextRcmdr(c("Leontief", "Wang-Wei-Zhu", 
                                       "Vertical Specialisation")), title = gettextRcmdr("Decomposition method"),
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
  tkgrid(getFrame(yBox), sticky = "nw")
  tkgrid(getFrame(kBox), sticky = "nw")
  tkgrid(getFrame(iBox), sticky = "nw")
  tkgrid(getFrame(oBox), sticky = "nw")
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