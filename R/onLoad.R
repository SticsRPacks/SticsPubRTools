.onLoad <- function(libname, pkgname...) {
  # Creating the stics environment
  #invisible(SticsRFiles::sticsenv())

  # Updating files for the V10.0 version, inputs.csv and outputs.csv
  suppressMessages(
    SticsRFiles:::update_stics_version(
      url = "https://w3.avignon.inra.fr/svn/modulostics/branches/modulo_stics_perennes" ,
      version_name = "V10.0"))
}
