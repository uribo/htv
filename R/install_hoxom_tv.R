#' Install package from hoxom-task view list
#' 
#' @importFrom ctv read.ctv
#' @importFrom pforeach npforeach
#' @importFrom devtools install_github
#' @name install_hoxom_tv
#' @export
#' @examples 
#' \dontrun{
#' install_hoxom_tv()
#' }
install_hoxom_tv <- function() {
  tv <- ctv::read.ctv(system.file("Hoxom.ctv", package = "htv"))
  pforeach::npforeach(i = 1:nrow(tv$packagelist))({
    devtools::install_github(tv$packagelist$name[[i]])
  })
}





