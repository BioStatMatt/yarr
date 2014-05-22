PROJECT=<<=cat(project)>>
YARRDIR=yarr
RSTDDIR=rst
HTMLDIR=html
RSRCDIR=R
DATADIR=data

HTML=$(addprefix $(HTMLDIR)/,index.html)
RSTD=$(addprefix $(RSTDDIR)/,index.rst)
RSRC=$(addprefix $(RSRCDIR)/,common.R)

# PRODHOST - web host address
PRODHOST=<<=cat(remote_host)>>
# PRODUSER - web host username 
PRODUSER=<<=cat(remote_user)>>
# PRODDIR - web host directory 
PRODDIR=<<=cat(remote_path)>>
# PRODURL - the complete url 
PRODURL=<<=cat(remote_url)>>
# RSYNC - rsync command
RSYNC=/usr/bin/rsync -r -v
# BROWSER - web browser
BROWSER=/usr/bin/firefox
# RSTARGS - rst2html arguments
RSTSTYL=--stylesheet-path='html/style.css'
# RSTSTYL - rst2html stylesheet
RSTARGS=--no-toc-backlinks --initial-header-level=2

$(RSTDDIR)/%.rst:	$(YARRDIR)/%.rst.R $(RSRC)
			R --vanilla -e 'library("yarr"); yarr("$<",output="$@")'

$(HTMLDIR)/%.html:	$(RSTDDIR)/%.rst $(HTMLDIR)/style.css
			rst2html $(RSTSTYL) $(RSTARGS) $< > $@ 

build:			$(HTML)

burn:
			rm -f $(HTML)
			rm -f $(HTMLDIR)/cache/*
			rm -f $(DATADIR)/cache/*

clean:

publish:		build
			$(RSYNC) $(HTMLDIR)/ $(PRODUSER)@$(PRODHOST):$(PRODDIR)

view-remote:		publish
			$(BROWSER) $(PRODURL)

view-local:		build
			$(BROWSER) $(HTMLDIR)/index.html

view:			view-local

