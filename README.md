# racas
 
## Installing customer tag

...from a user with install global privs (root)

    curl -O --digest --user username:password https://bitbucket.org/mcneilco/racas/get/dns-avocet-1.0.tar.gz
    Rscript -e "install.packages(\"dns-avocet-1.0.tar.gz\", repo = NULL)"
    
    
## Installing customer branch

...from an R session...

    if (!require('devtools')) {
          install.packages('devtools')
    }
    library(devtools)
    library(httr)

...for installing master (change username/password to your git username/password)...

    install_url(url = "https://bitbucket.org/mcneilco/racas/get/master.zip", config = authenticate(user = "username", password = "password", type = "basic"))
        or
    install_bitbucket(repo = "racas", username = "mcneilco", ref = "master", auth_user = "username", password = "password")

...for a branch (change username/password to your git username/password)...

    install_url(url = "https://bitbucket.org/mcneilco/racas/get/dns.zip", config = authenticate(user = "username", password = "password", type = "basic"))
        or
    install_bitbucket(repo = "racas", username = "mcneilco", ref = "dns", auth_user = "username", password = "password")


##Notes:

You must restart apache if you are looking for changes in curve rendering through rapche.