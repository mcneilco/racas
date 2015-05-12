FROM mcneilco/centos-node-r

USER	root

RUN	useradd -u 1000 -ms /bin/bash runner

RUN	yum -y localinstall http://yum.postgresql.org/9.4/redhat/rhel-6-x86_64/pgdg-centos94-9.4-1.noarch.rpm && \
	yum install -y postgresql94-devel curl-devel libxml2-devel

COPY	. /home/runner/racas
RUN  Rscript -e 'options(repos = c(CRAN="http://cran.rstudio.com/"));install.packages("miniCRAN");source("/home/runner/racas/R/installation.R");makeRepo(path="/home/runner/repo", description = "/home/runner/racas/DESCRIPTION", racasPath = "/home/runner/racas")'
RUN	 mkdir /home/runner/r_libs
RUN  Rscript -e 'install.packages("racas", repos="file://home/runner/repo", lib = "/home/runner/r_libs")'
RUN  rm -rf /home/runner/repo /home/runner/racas /home/runner/r_libs/racas
