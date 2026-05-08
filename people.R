##' Store data of a SMiG member
##'
##' @param first First name
##' @param last Last name
##' @param dima Are they part of the Dept. of Mathematics at UniGe? It is only needed to infer the email address and the webpage
##' @param phd Are they a phd student? It is only needed to infer the role.
##' @param former Are they a former member?
##' @param role Role or job title
##' @param affiliation Affiliation or work place
##' @param email Email
##' @param website Personal website
##' @param comment Extra field
##' @return A character vector with their personal information.
##' @author Marco Bressan
smig.person <- function (first, last, dima = !former, phd = FALSE, former = FALSE,
                         role = if (phd) "PhD student",
                         affiliation = if (dima) "University of Genova",
                         email,
                         website = if (dima) sprintf("https://dima.unige.it/%s", email),
                         comment = NULL) {
  if (missing(email))
    email = if (dima) sprintf("%s.%s@%sunige.it", first, last,
                              if (phd) "edu." else "")
  c(first = first, last = last, former = former, role = role, affiliation = affiliation,
    email = email, website = website, comment = comment)
}
##' Convert a SMiG person representation to string.
##'
##' @param p A SMiG person created with `smig.person()`
##' @param style The printing style. Only "simple" currently available.
##' @return The character representation of `p`.
##' @author Marco Bressan
format.smig.person <- function(p, style = "simple") {
  style <- match.arg(style)
  if (anyNA(p[c("first", "last")]))
    stop("Not a SMiG person!")
  if (style == "simple") {
    affil.str <- ""
    if (!is.na(p["affiliation"])) {
      if (!is.na(p["role"]))
        affil.str <- paste(p[c("role", "affiliation")], collapse = " at ")
      else
        affil.str <- p["affiliation"]
    }
    nm <- sprintf("**%s %s**", p["first"], p["last"])
    if (!is.na(p["website"]))
      nm <- sprintf("[%s](%s)", nm, p["website"])
    return(
      paste0(nm,
             if (nzchar(affil.str)) sprintf(", %s", affil.str), ".",
             if (!is.na(p["email"])) sprintf(" [Email](mailto:%s).", p["email"]))
    )
  }
}
