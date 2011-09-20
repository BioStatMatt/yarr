#  yarr - Mixing R Output with Text
#
#  Copyright (C) 2011 Matthew S. Shotwell
#
#  yarr is free software; licensed under the terms of the GNU General Public
#  License; either version 2 of the License, or any later version.
#  http://www.gnu.org
#
#  This program is distributed WITHOUT ANY WARRANTY nor the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

# Escape reserved HTML characters
escape_HTML <- function(text) {
    text <- gsub('&', '&amp;', text)
    text <- gsub('"', '&quot;', text)
    text <- gsub("'", '&apos;', text)
    text <- gsub('>', '&gt;', text)
            gsub('<', '&lt;', text)
}

# Code handlers with prototype: function(code, envir, ...), where code is a
# character vector of length one, and envir is the environment where evaluation
# should occur. By convention, handlers strip trailing newlines.

capture_handler_evaluate <-
function(code, envir, output=TRUE, source=TRUE, prompt=TRUE) {
    require("evaluate", quietly=TRUE)
    code <- sub('^@','',code)
    code <- sub('^\\n','',code)
    res <- evaluate(code, envir)
    out <- ''
    for(val in res) {
        if(is.character(val) && output)
            out <- paste(out,val,sep='',collapse='')
        if(is.source(val) && output && source)
            out <- paste(out,line_fmt(val$src, prompt),sep='',collapse='')
        if(is.warning(val))
            out <- paste(out,'Warning: ',val$message,sep='')
        if(is.message(val))
            out <- paste(out,'Message: ',val$message,sep='')
        if(is.error(val))
            out <- paste(out,'Error: ',val$message,sep='')
        if(is.recordedplot(val))
            assign('.recordedplot',val,envir=envir)
    }
    out <- sub('\\n$','',out)
    return(out)
}

capture_handler_classic <-
function(code, envir, output=TRUE, source=TRUE, prompt=TRUE) {
    code <- sub('^@','',code)
    exp <- try(parse(text=code),TRUE)
    if(inherits(exp, 'try-error'))
        return(exp) 
    out <- ''
    if(length(exp) == 0) return(out)
    for(i in 1:length(exp)) {
        dep <- deparse(exp[[i]])
        res <- try(capture.output(eval(exp[i],envir)),TRUE)
        if(output) {
            if(source)
                out <- paste(out, line_fmt(dep, prompt), sep='')
            if(length(res)>0) {
                res <- paste(res, collapse='\n', sep='')
                #if(!grepl('\\n$', res))
                    res <- paste(res, '\n', sep='')
                out <- paste(out, res, sep='')
            }
        }
    }
    out <- sub('\\n$', '', out)
    return(out)
}

capture_handler <-
function(code, envir, output=TRUE, source=TRUE, prompt=TRUE) {
    if(suppressWarnings(require("evaluate", quietly=TRUE))) {
        capture_handler_evaluate(code, envir, output, source, prompt)
    } else {
        capture_handler_classic(code, envir, output, source, prompt)
    }
}

silent_handler <- function(code, envir) {
    capture_handler(code, envir, output=FALSE)
}

result_handler <- function(code, envir) {
    code <- sub('^=','',code)
    capture_handler(code, envir, source=FALSE)
}

source_handler <- function(code, envir) {
    code <- sub('^&','',code)
    capture_handler(code, envir, prompt=FALSE)
}

line_fmt <- function(x, prompt=TRUE) {
    lines <- unlist(strsplit(x, '\n'))
    if(prompt) {
        lines[1] <- paste(options('prompt'), lines[1], sep='')
        if(length(lines) > 1)
            lines[-1] <- paste(options('continue'), lines[-1], sep='')
    }
    paste(lines, '\n', collapse='', sep='')
}

html_result_handler <- function(code, envir) {
    code <- sub('^/','',code)
    escape_HTML(result_handler(code,envir))
}

html_source_handler <- function(code, envir) {
    code <- sub('^/','',code)
    escape_HTML(source_handler(code,envir))
}

html_capture_handler <- function(code, envir) {
    code <- sub('^/','',code)
    escape_HTML(capture_handler(code,envir))
}

default_handlers <- function() {
    handlers <- list()
    handlers[[1]] <- list(regex='',handler=silent_handler)
    handlers[[2]] <- list(regex='^=',handler=result_handler)
    handlers[[3]] <- list(regex='^&',handler=source_handler)
    handlers[[4]] <- list(regex='^@',handler=capture_handler)
    handlers[[5]] <- list(regex='^/=',handler=html_result_handler)
    handlers[[6]] <- list(regex='^/&',handler=html_source_handler)
    handlers[[7]] <- list(regex='^/@',handler=html_capture_handler)
    return(handlers)
}

#other symbols that are not syntactically valid at the 
#beginning of an expression: '&', '*', '%', '<' '>'

#This closing delimiter regex permits removal of trailing
#newlines in the style of the brew package.
default_delim <- function() c('<<', '>>|->>|->>\n')

dispatch <- function(code, envir) {
    handlers <- list()
    if(exists(".handlers", envir))
        handlers <- get(".handlers", envir)
    hdl <- function(code, envir) 
        return("Error: no handler found")
    # Search for handler
    for(type in handlers) 
        if(grepl(type$regex, code))
            hdl <- type$handler
    # Call handler
    try(as.character(hdl(code, envir)),TRUE)
}

yarr <- function(file=stdin(),envir=parent.frame(),output=stdout(),text=NULL,
         delim=default_delim(),handlers=default_handlers()) {

    closeIcon <- closeOcon <- FALSE

    # Check file, text
    if (is.character(text) && length(text) > 0) {
        closeIcon <- TRUE
        icon <- textConnection(text)
    } else if (inherits(file,'connection') && isOpen(file,"read")) {
        icon <- file
    } else if (is.character(file)) {
        closeIcon <- TRUE
        icon <- file(file,open="rt")
    } else {
        if(!is.null(text))
            stop("\'text\' must be NULL or non-empty character vector")
        stop("\'file\' is not valid")
    }

    # Check output
    if (inherits(output,'connection') && isOpen(output,"write")) {
        ocon <- output
    } else if ( is.character(output) ) {
        closeOcon <- TRUE
        ocon <- file(output,open="wt")
    } else {
        stop("\'output\' is not valid")
    }

    # Check envir
    if (!is.environment(envir))
        stop("\'envir\' is not an environment")

    # Check delim
    if(!is.character(delim) || length(delim) != 2)
        stop("\'delim\' must be a character vector of length two")
    if(nchar(delim[1]) < 1 || nchar(delim[2]) < 1)
        stop("\'delim\' items must not be empty")

    # Check handlers
    if(!is.list(handlers))
        stop("\'handlers\' must be a list")
    for(hdl in handlers) {
        if(!is.list(hdl))
            stop("each handler must be a list")
        if(!all(c("regex", "handler") %in% names(hdl)))
            stop("each handler must have named elements \'regex\' and \'handler\'")
        if(!is.character(hdl$regex) || length(hdl$regex) != 1)
            stop("\'regex\' must be a character vector of length 1")
        if(!is.function(hdl$handler) || length(formals(hdl$handler)) < 2)
            stop("\'handler\' must be a function accepting two or more arguments")
    }

    assign(".handlers", handlers, envir)

    YCODE  <- 0   
    YTEXT  <- status <- 1
    code   <- line  <- '' 
    input  <- list()
    mlen   <- function(regex) attr(regex, "match.length")

    while(TRUE) {

        # Read line, add back '\n', break on EOF
        if(line == '') {
            line <- readLines(icon, 1)
            if(length(line) < 1) break
            line <- paste(line,'\n',sep='')
        }

        if(status == YTEXT) {
            # Look for opening delimiter
            odel <- regexpr(delim[1], line)
            if(odel < 0) {
                writeLines(line, ocon, sep='')
                input[[length(input) + 1]] <- list(type='text',data=line)
                line <- ''
            } else {
                writeLines(substr(line, 1, odel-1), ocon, sep='')
                line <- substr(line, odel + mlen(odel), nchar(line))
                status <- YCODE
            } 
            next
        }
        
        if(status == YCODE) {
            # Look for closing delimiter
            cdel <- regexpr(delim[2], line)
            if(cdel < 0) {
                code <- paste(code, line, sep='')
                line <- ''
            } else {
                code <- paste(code, substr(line, 1, cdel-1), sep='')
                writeLines(dispatch(code, envir), ocon, sep='')
                input[[length(input) + 1]] <- list(type='code',data=code)
                code <- ''
                line <- substr(line, cdel + mlen(cdel), nchar(line))
                status <- YTEXT
            }
        }
    }
    
    if(closeIcon) close(icon)
    if(closeOcon) close(ocon)
    if(status == YCODE)
        warning("file ended unexpectedly")
    
    invisible(input)
}
