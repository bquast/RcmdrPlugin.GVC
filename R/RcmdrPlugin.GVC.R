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
  defaults <- list (initial.x = NULL, initial.y = NULL, initial.method = "leontief" )
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
    method <- as.character(tclvalue(methodVariable))
    putDialog ("gvc_decomp", list (initial.x = x, initial.method = method) )
    closeDialog()
    doItAndPrint(paste("with(", ActiveDataSet (), ", (t.test(", x, 
                       ", method='", method, "'", sep = ""))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "decomp", reset = "gvc_decomp")
  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "method", buttons = c("leontief", 
                                                               "wwz", "vertical_specialisation"), values = c("leontief", "wwz", "vertical_specialisation"), 
               labels = gettextRcmdr(c("Leontief", "Wang-Wei-Zhu", 
                                       "Vertical Specialisation")), title = gettextRcmdr("Decomposition method"),
               initialValue = dialog.values$initial.method)
  rightFrame <- tkframe(optionsFrame)
  tkgrid(getFrame(xBox), sticky = "nw")
  tkgrid(getFrame(yBox), sticky = "nw")
  tkgrid(getFrame(kBox), sticky = "nw")
  tkgrid(getFrame(iBox), sticky = "nw")
  tkgrid(getFrame(oBox), sticky = "nw")
  tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
  tkgrid(methodFrame, rightFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
  

}