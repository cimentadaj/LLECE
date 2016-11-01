# This function uses unRAR tools for Mac OSX to
# unrar files directly from command line

# The user must have it installed by following
# these steps: http://best-mac-tips.com/2013/02/01/install-free-command-line-unrar-mac/

# rarfile = exact path to the rar file
# exdir = directory to be extracted

unrar <- function(rarfile, exdir) {
  call <- paste("cd", exdir,"\n", "unrar x", rarfile, sep = " ")
  system(call)
}
