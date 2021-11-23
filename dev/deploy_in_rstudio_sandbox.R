


for(i in 1:length(renv_lock$Packages)){


  if(source=="GitHub"){
    RemoteUsername <- package[[1]]$RemoteUsername
    RemoteRepo <- package[[1]]$RemoteRepo
    RemoteRef <- package[[1]]$RemoteRef

    package.file.name <- paste0(name, "_", version, ".tar.gz")
    package.dest <- file.path(path_output_folder, package.file.name)
    package.file.name_tmp <- paste0(name, "_", version, ".zip")
    package.dest_tmp <- file.path(path_output_folder, package.file.name)

    heads_tags <- ifelse(RemoteRef=="master", "heads", "tags")

    package.url <- paste0(github_url, "/", RemoteUsername, "/", RemoteRepo,
                          "/archive/", heads_tags, "/", RemoteRef, ".zip")


    download.file(package.url, package.dest_tmp)

    # unzip and tar
    unzip(package.dest_tmp, exdir = path_output_folder)

    tar(tarfile = package.file.name,
        files = paste0(path_output_folder, "/", RemoteRepo, "-", heads_tags,  "-", RemoteRef  ),
        compression = "gzip")


    status_tibble <- bind_rows(status_tibble, tibble(package=name, version=version, source= source, source_type=heads_tags))
  }

}
