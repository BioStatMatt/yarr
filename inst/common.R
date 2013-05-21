html.data.frame <- function(object, header='', footer='',
    rownames=TRUE, fontsize=10, ...) {
    esc <- function (text) {
        text <- gsub("&", "&amp;", text)
        text <- gsub("\"", "&quot;", text)
        text <- gsub("'", "&apos;", text)
        text <- gsub(">", "&gt;", text)
        gsub("<", "&lt;", text)
    }
    row_count <- nrow(object)
    col_count <- ncol(object)
    row_names <- gsub(" ", "&nbsp;", esc(row.names(object)))
    col_names <- gsub(" ", "&nbsp;", esc(names(object)))
    cat(header, "<table style=\"font-size:",fontsize,"pt;\"><tr><th></th><th>",
         paste(col_names, collapse="</th><th>"), "</th></tr>", sep="")
    evenodd <- "e"
    for(i in 1:row_count) {
        cat("<tr class=\"",evenodd,"\"><td>", 
            ifelse(rownames, row_names[i], ""),
            "</td><td>", paste(format(object[i,], ...),
            collapse = "</td><td>"), "</td></tr>", sep="")
        evenodd <- ifelse(evenodd=="e", "o", "e")
    }
    if(!is.null(attr(object, 'caption')))
        cat("<tr class=\"c\"><td colspan=\"",col_count+1,"\">",
            attr(object, 'caption'),"</td></tr>", sep="")
    cat("</table>",footer)
}

html.trellis <- function(object, basename=deparse(substitute(object)),
    basepath="html/cache/", baseurl="cache/", pngd=c(500,500), pdfd=c(7,7)) {
    basename <- gsub('[[:punct:]][[:space:]]', '', basename)
    pngfile <- paste(basepath, basename, '.png', sep='')
    pngurl  <- paste(baseurl, basename, '.png', sep='')
    trellis.device(png, file=pngfile, height=pngd[1], width=pngd[2],
        type="cairo")
    print(object)
    dev.off()
    pdffile <- paste(basepath, basename, '.pdf', sep='')
    pdfurl  <- paste(baseurl, basename, '.pdf', sep='')
    trellis.device(pdf, file=pdffile, height=pdfd[1], width=pdfd[2])
    print(object)
    dev.off()
    cat('<a href="', pdfurl, '"><img class="r" src="', pngurl,
        if (pngd[2] > 800) 'width="800"></a>' else '"></a>', sep='')
}

html.basegr <- function(expr, basename=deparse(substitute(object)),
    basepath="html/cache/", baseurl="cache/", pngd=c(500,500), pdfd=c(7,7)) {
    pngfile <- paste(basepath, basename, '.png', sep='')
    pngurl  <- paste(baseurl, basename, '.png', sep='')
    png(file=pngfile, height=pngd[1], width=pngd[2],
        type="cairo")
    eval(expr)
    dev.off()
    pdffile <- paste(basepath, basename, '.pdf', sep='')
    pdfurl  <- paste(baseurl, basename, '.pdf', sep='')
    pdf(file=pdffile, height=pdfd[1], width=pdfd[2])
    eval(expr)
    dev.off()
    cat('<a href="', pdfurl, '"><img class="r" src="', pngurl,
       if (pngd[2] > 800) 'width="800"></a>' else '"></a>', sep='')
}
    
