PROJECT=<<=cat(project)>>
YARRDIR=yarr
HTMLDIR=html
RSRCDIR=R
DATADIR=data

HTML=$(addprefix $(HTMLDIR)/,index.html)
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

$(HTMLDIR)/%.html:	$(YARRDIR)/%.html.R $(RSRC) $(HTMLDIR)/style.css
			R --vanilla -e 'yarr::yarr("$<",output="$@")'

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
