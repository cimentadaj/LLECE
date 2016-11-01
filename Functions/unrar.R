# This function uses unRAR tools for Mac OSX to
# unrar files directly from command line
# The user must have it installed by following
# these steps: http://best-mac-tips.com/2013/02/01/install-free-command-line-unrar-mac/
# zipfile = exact path to the zip file
# exdir = directory to be extracted

unrar <- function(zipfile, exdir) {
  call <- paste("cd", exdir,"\n", "unrar x", zipfile, sep = " ")
  print(call)
  system(call)
}
