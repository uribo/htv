#' Generare Hoxom task view for render as html.
#' @description Wrapper function of ctv::ctv2html()
#' @import ctv
#' @param file character
#' @name htv2html
#' @examples 
#' \dontrun{
#' htv2html()
#' httr::BROWSE("Hoxom.html")
#' }
htv2html <-
  function (file = NULL)
  {
    x <- ctv::read.ctv(system.file("Hoxom.ctv", package = "htv"))
    if (is.null(file))
      file <- paste0(x$name, ".html")
    ampersSub <- function(x)
      gsub("&", "&amp;", x)
    obfuscate <-
      function(x)
        paste(sprintf("&#x%x;", as.integer(sapply(
          unlist(strsplit(gsub("@",
                               " at ", x), NULL)), charToRaw
        ))), collapse = "")
    strip_encoding <- function(x) {
      if (is.character(x))
        Encoding(x) <- "unknown"
      return(x)
    }
    for (i in 1:length(x))
      x[[i]] <- strip_encoding(x[[i]])
    title <- paste0("HOXOM Task View: ", ctv:::htmlify(x$topic))
    htm1 <-
      c(
        "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">",
        "<html xmlns=\"http://www.w3.org/1999/xhtml\">",
        "<head>",
        paste0("  <title>", title, "</title>"),
        paste0(
          "  <link rel=\"stylesheet\" type=\"text/css\" href=\"",
          "../CRAN_web.css",
          "\" />"
        ),
        "  <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />",
        sprintf("  <meta name=\"citation_title\" content=\"%s\" />",
                title),
        sprintf(
          "  <meta name=\"citation_author\" content=\"%s\" />",
          ctv:::htmlify(x$maintainer)
        ),
        sprintf(
          "  <meta name=\"citation_publication_date\" content=\"%s\" />",
          x$version
        ),
        sprintf(
          "  <meta name=\"citation_public_url\" content=\"http://CRAN.R-project.org/view=%s\" />",
          x$name
        ),
        sprintf("  <meta name=\"DC.title\" content=\"%s\" />",
                title),
        sprintf(
          "  <meta name=\"DC.creator\" content=\"%s\" />",
          ctv:::htmlify(x$maintainer)
        ),
        sprintf("  <meta name=\"DC.issued\" content=\"%s\" />",
                x$version),
        "</head>",
        "",
        "<body>",
        paste0("  <h2>",
               " Task View: ",
               ctv:::htmlify(x$topic),
               "</h2>"),
        paste0("  <table summary=\"", x$name, " task view information\">"),
        paste0(
          "    <tr><td valign=\"top\"><b>Maintainer:</b></td><td>",
          ctv:::htmlify(x$maintainer),
          "</td></tr>"
        ),
        if (!is.null(x$email))
          paste0(
            "    <tr><td valign=\"top\"><b>Contact:</b></td><td>",
            obfuscate(x$email),
            "</td></tr>"
          ),
        paste0(
          "    <tr><td valign=\"top\"><b>Version:</b></td><td>",
          ctv:::htmlify(x$version),
          "</td></tr>"
        ),
        "  </table>"
      )
    htm2 <- x$info
    pkg2html <- function(a, b)
      paste0(
        "    <li><a href=\"",
        "https://github.com/",
        a,
        "\">",
        a,
        "</a>",
        if (b)
          " (core)"
        else
          "",
        "</li>"
      )
    htm3 <- c(
      paste0("  <h3>Source on GitHub repository:</h3>"),
      "  <ul>",
      sapply(1:NROW(x$packagelist), function(i)
        pkg2html(x$packagelist[i,
                               1], x$packagelist[i, 2])),
      "  </ul>"
    )
    htm4 <- c("  <h3>Related links:</h3>",
              "  <ul>",
              sapply(x$links,
                     function(x)
                       paste0("    <li>", x, "</li>")),
              "  </ul>")
    htm <- c(htm1, "", htm2, "", htm3, "", htm4, "", "</body>",
             "</html>")
    writeLines(htm, con = file)
    invisible(htm)
  }
