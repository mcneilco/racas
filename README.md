# racas
 
 
## Installing racas

Usually done in /opt

    library(devtools)
    library(httr)

for master

    install_url(url = "https://bitbucket.org/mcneilco/racas/get/master.zip", config = authenticate(user = "username", password = "password", type = "basic"))

for a branch

    install_url(url = "https://bitbucket.org/mcneilco/racas/get/dns.zip", config = authenticate(user = "username", password = "password", type = "basic"))
