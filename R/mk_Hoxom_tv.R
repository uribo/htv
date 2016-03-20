#' Making Hoxom task view
#' @import magrittr
#' @import XML
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom githubinstall list_github_packages
#' @importFrom pforeach npforeach
#' @importFrom gh gh
#' @importFrom lambdaR Map_
#' @importFrom lambdaR Reduce_
#' @name mk_Hoxom_tv
#' @examples 
#' \dontrun{
#' mk_Hoxom_tv()
#' 
#' mk_Hoxom_tv() %>% saveXML(file = "Hoxom.ctv")
#' }
mk_Hoxom_tv <- function() {

  data("hoxompkgs")
  .gh <- "https://github.com/"
  .tv <- XML::newXMLNode("CRANTaskView")
  XML::newXMLNode("name", "Hoxom", parent = .tv)
  XML::newXMLNode("topic",
                  "HOXOM that Japanese Secret Society of Data Science",
                  parent = .tv)
  XML::newXMLNode(
    "maintainer",
    "Shinya Uryu",
    attrs = c(email = "suika1127@gmail.com"),
    parent = .tv
  )
  XML::newXMLNode("version", as.character(Sys.Date()), parent = .tv)
  .info <-
    XML::newXMLNode("info",
                    "We are 'HOXO-M' anonymous data analysis and R user group in Japan!!!",
                    parent = .tv)
  
  .info.ul <- XML::newXMLNode("ul", parent = .info)
  
  for (i in 1:nrow(hoxompkgs)) {
    .info.li <- XML::newXMLNode("li", parent = .info.ul)
    XML::newXMLNode(
      "a",
      hoxompkgs$full_name[[i]] %>% gsub(".+/", "", .),
      attrs = c(href = paste0(.gh, hoxompkgs$full_name[[i]])),
      parent = .info.li
    )
    XML::newXMLTextNode(paste0(
      "by ",
      hoxompkgs$full_name[[i]] %>% gsub("/.+", "", .),
      ifelse(
        is.na(hoxompkgs$description[[i]]),
        "",
        paste0(": ", hoxompkgs$description[[i]])
      )
    ), parent = .info.li)
  }
  
  .pkgs <- XML::newXMLNode("packagelist", parent = .tv)
  
  for (i in 1:nrow(hoxompkgs)) {
    XML::newXMLNode("gh", hoxompkgs$full_name[[i]], parent = .pkgs)
  }
  
  .links <- XML::newXMLNode("links", parent = .tv)
  XML::newXMLNode(
    "a",
    "List of members",
    attrs = c(href = "https://twitter.com/teramonagi/lists/list/members"),
    parent = .links
  )
  .tv
}


