clearConsole = clc = function(){
  # Clear console messages
  cat( "\014" )
}

clearPlots = function () {
  # Clear plots
  if( dev.cur() > 1 ) dev.off()
}

clearWorkspace = function () {
  # Clear global workspace
  rm( list = ls( envir = globalenv() ), envir = globalenv() )
}

clearAll = function(){
  # Clear console, plots, and workspace
  clearConsole()
  clearPlots()
  clearWorkspace()
}