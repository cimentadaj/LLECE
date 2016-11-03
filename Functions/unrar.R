# This function uses unRAR tools for Mac OSX to
# unrar files directly from command line

# The user must have it installed by following
# these steps: http://best-mac-tips.com/2013/02/01/install-free-command-line-unrar-mac/

# rarfile = exact path to the rar file
# exdir = directory to be extracted
# password = password of the rar file

unrar <- function(rarfile, exdir, password = NULL) {
  # If the password is empty, return nothing, if it's not
  # return the password with the unrar argument for password
  # all wrapped in quotes
  pw <- ifelse(!is.null(password), shQuote(paste0("-p", password)), "")
  
  # Quote all strings
  rarfile <- shQuote(rarfile)
  exdir <- shQuote(exdir)
  
  # combine everything into a single call:
  # first the command, second the rar path, then the extract
  # path and final the password. Note that if the password
  # is NULL it won't affect the call. If it's not NULL
  # then it will open the rar file.
  system2("unrar", c(paste("x", rarfile, sep = " "), exdir, pw))
}
