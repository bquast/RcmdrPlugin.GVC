#' Interface function for decompositions

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

RcmdrPlugin.decompr.ltv <- function(){
  require(TeachingDemos)
  use.rgl <- options("Rcmdr")[[1]]$use.rgl
  if (length(use.rgl) == 0 || use.rgl) require(rgl)
  rgl.open()
  rgl.coin()
  flip.rgl.coin()
}