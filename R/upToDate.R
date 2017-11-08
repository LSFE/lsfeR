

require(installr)

upToDate <- function() {
  
  cv <- check.for.updates.R(GUI=FALSE) # check if an update is needed
  rv <- getRversion() # current R version
  updateR(F, T, T, F, T, F, T) # # install, move, update.package, quit R.
  # use newest R version as default
  
  # unistall old R version
  if (cv) {unistall.R(rv, GUI=FALSE)}
  
  pkg_dirs <- get.installed.R.folders() # installed R version(s)
  pkg_names <- names(pkg_dirs) # R version(s) code name(s)
  si <- sort(pkg_names, index.return=TRUE)$ix[1] # sort newest to olderst
  pkg_dirs <- pkg_dirs[si] # update package list (get latest)
  pkg_names <- pkg_names[si] # update name list (get latest)
  
  
  
  
  .libPaths()
  Sys.getenv("R_HOME") # R instalation folder
  Sys.getenv('HOME') # Rstudio folder location
  Sys.getenv('R_LIBS_USER') # used library
  basename(Sys.getenv('R_LIBS_USER')) # R version
  paste0(Sys.getenv('HOME'), '/RStudio/bin/Rstudio.exe') # executable
  
  
  
}

