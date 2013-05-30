# racas
 
 
## Installing racas

...from an R session...

    if (!require('devtools')) {
          install.packages('devtools')
    }
    library(devtools)
    library(httr)

...for installing master (change username/password to your git username/password)...

    install_url(url = "https://bitbucket.org/mcneilco/racas/get/master.zip", config = authenticate(user = "username", password = "password", type = "basic"))

...for a branch (change username/password to your git username/password)...

    install_url(url = "https://bitbucket.org/mcneilco/racas/get/dns.zip", config = authenticate(user = "username", password = "password", type = "basic"))
