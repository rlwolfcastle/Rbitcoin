.onLoad <- function(libname, pkgname){
  options(Rbitcoin.verbose=0)
  options(RCurlOptions=list(ssl.verifypeer = TRUE, 
                            ssl.verifyhost = TRUE, 
                            cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"), 
                            verbose = FALSE))
  invisible()
}