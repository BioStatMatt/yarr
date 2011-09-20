<<
    # Add a new handler to cite R
    .handlers[[length(.handlers) + 1]] <- list(
        regex = '^citation$',
        handler = function(code, envir) {
            cit <- attr(citation()[[1]], 'textVersion')
            paste(strwrap(cit, width=70), sep='', collapse='\n')
        })
->>
This is how R should be cited in publications:
<<citation>>
Here it is again:
<<citation>>
