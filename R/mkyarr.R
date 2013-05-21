#  yarr - Mixing R Output with Text
#
#  Copyright (C) 2013 Matthew S. Shotwell
#
#  yarr is free software; licensed under the terms of the GNU General Public
#  License; either version 2 of the License, or any later version.
#  http://www.gnu.org
#
#  This program is distributed WITHOUT ANY WARRANTY nor the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

mkyarr <- function(project, remote_host="", remote_user="",
                   remote_path="", remote_url ="", verbose=FALSE) {

    # store old warn
    warn <- options("warn")
    # set to error on warning if unable
    # to create base project directory
    options(warn=2)
    dir.create(project)
    # restore old warn
    options(warn)
    if(verbose)
        cat(paste(project, "created\n"))

    # create sub-directories
    subdirs <- c("yarr", "data", "html", "R", "docs",
                 file.path("html", "cache"),
                 file.path("data", "cache")
                )

    for(sd in subdirs) {
        dirpath <- file.path(project, sd)
        dir.create(dirpath)
        if(verbose)
            cat(paste(dirpath, "created\n"))
    }

    # copy templates and common files
    copyfiles <- list(
        list(from="index.html.R",
             to=file.path(project, "yarr", "index.html.R")),
        list(from="common.R",
             to=file.path(project, "R", "common.R")),
        list(from="style.css",
             to=file.path(project, "html", "style.css")),
        list(from="README",
             to=file.path(project, "README"))
        )

    for(cf in copyfiles) {
        file.copy(system.file(cf$from, package="yarr"), cf$to)
        if(verbose)
            cat(paste(cf$to, "created\n"))
    }

    # create Makefile
    yarr::yarr(system.file("Makefile.R", package="yarr"),
               output=file.path(project, "Makefile"))
    if(verbose)
        cat(paste(file.path(project, "Makefile"), "created\n"))
}
