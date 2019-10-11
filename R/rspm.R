
renv_rspm_repos <- function() {

  repos <- getOption("repos")
  binrepos <- lapply(repos, function(repo) {
    catch(renv_rspm_repos_binary(repo))
  })

}

renv_rspm_repos_binary <- function(repo) {

  api <- file.path(repo, "__api__/status")
  destfile <- tempfile("renv-status-", fileext = ".json")
  status <- renv_download_impl(api, destfile)
  json <- renv_json_read(destfile)

  codename <- renv_rspm_codename()
  for (distro in json$binary_distros) {
    if (distro$url == codename) {
      binpath <- file.path(repo, "__linux__", codename, "latest")
      return(binpath)
    }
  }

}

renv_rspm_codename <- function() {

  release <- NULL
  for (file in c("/etc/os-release", "/etc/redhat-release")) {
    if (file.exists(file)) {
      props <- renv_read_properties(file, delimiter = "=")
      release <- lapply(props, function(prop) {
        as.character(parse(text = prop, keep.source = FALSE))
      })
      break
    }
  }

  # check for opensuse
  if (identical(release[["ID"]], "opensuse-leap"))
    return("opensuse15")

  # check for centos
  if (identical(release[["ID"]], "centos"))
    return(paste0("centos", release[["VERSION_ID"]]))

  # check for Ubuntu variants
  codename <- release[["UBUNTU_CODENAME"]] %||% release[["VERSION_CODENAME"]]
  if (!is.null(codename))
    return(codename)

}
