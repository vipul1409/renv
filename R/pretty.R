
renv_pretty_print <- function(values,
                              preamble = NULL,
                              postamble = NULL,
                              emitter = NULL,
                              wrap = TRUE)
{
  msg <- stack()

  if (!is.null(preamble)) {
    msg$push(paste(preamble, collapse = "\n"))
    msg$push("")
  }

  formatted <- if (wrap)
    strwrap(paste(values, collapse = ", "), width = 60)
  else
    values

  msg$push(paste("\t", formatted, sep = "", collapse = "\n"))

  if (!is.null(postamble)) {
    msg$push("")
    msg$push(paste(postamble, collapse = "\n"))
  }

  msg$push("")
  text <- paste(as.character(msg$data()), collapse = "\n")

  emitter <- emitter %||% writeLines
  emitter(text)
}

renv_pretty_print_records <- function(records,
                                      preamble = NULL,
                                      postamble = NULL)
{
  formatted <- named(
    sprintf("  [%s]", map_chr(extract(records, "Version"), format)),
    sprintf("  %s",   map_chr(extract(records, "Package"), format))
  )

  preamble %&&% writeLines(preamble)
  print.simple.list(formatted)
  writeLines("")
  postamble %&&% writeLines(postamble)
  postamble %&&% writeLines("")

  invisible(NULL)
}

renv_pretty_print_records_pair <- function(before,
                                           after,
                                           preamble = NULL,
                                           postamble = NULL)
{
  if (!setequal(names(before), names(after)))
    stopf("internal error: names mismatch", call. = TRUE)

  nm <- renv_vector_intersect(names(before), names(after))
  before <- before[nm]; after <- after[nm]

  # compute groups
  labels <- mapply(function(lhs, rhs) {
    if (lhs[["Source"]] != rhs[["Source"]])
      "(Changed Source)"
    else if (!is.null(lhs[["Repository"]]))
      lhs[["Repository"]]
    else
      lhs[["Source"]]
  }, before, after)

  # report changes for each group
  pairs <- mapply(list, before, after, SIMPLIFY = FALSE)
  grouped <- split(pairs, labels)

  preamble %&&% writeLines(preamble)
  enumerate(grouped, renv_pretty_print_records_impl)
  postamble %&&% writeLines(postamble)
  postamble %&&% writeLines("")

  invisible(NULL)
}

renv_pretty_print_records_impl <- function(source, pairs) {

  lhs <- extract(pairs, 1L); rhs <- extract(pairs, 2L)

  # report remote fields if available
  # TODO: avoid hard-coding here
  remotes <- c("git", "github", "gitlab", "bitbucket")
  formatted <- if (source == "(Changed Source)") {

    fsrc <- function(item) item$Repository %||% item$Source
    fref <- function(item) item$RemoteRef %||% item$Version

    sprintf(
      "[%s::%s -> %s::%s]",
      map_chr(lhs, fsrc), map_chr(lhs, fref),
      map_chr(rhs, fsrc), map_chr(lhs, fref)
    )

  } else if (tolower(source) %in% remotes) {
    sprintf(
      "[%s: %s -> %s]",
      extract_chr(lhs, "RemoteRef"),
      substring(extract_chr(lhs, "RemoteSha"), 1L, 8L),
      substring(extract_chr(rhs, "RemoteSha"), 1L, 8L)
    )
  } else {
    sprintf(
      "%s -> %s",
      format(extract_chr(lhs, "Version")),
      format(extract_chr(rhs, "Version"))
    )
  }

  # add package names
  packages <- extract_chr(lhs, "Package")
  all <- paste(format(packages), formatted, sep = "  ")

  # print with header
  text <- c(header(source, 40L), all)
  writeLines(text)
  writeLines("")

}
