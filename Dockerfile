FROM centos:centos7

# R, Java
ENV JAVA_VERSION 1.8.0
ENV JAVA_HOME /usr/lib/jvm/java
ENV LD_LIBRARY_PATH $JAVA_HOME/jre/lib/amd64:$JAVA_HOME/jre/lib/amd64/server/
RUN echo "$JAVA_HOME/jre/lib/amd64/server/" > /etc/ld.so.conf.d/rApache_rJava.conf && \
    /sbin/ldconfig

RUN \
  useradd -u 1001 -ms /bin/bash builder && \
  yum update -y && \
  yum upgrade -y && \
  yum install -y epel-release && \
  yum install -y rpm-build which make wget tar httpd-devel libapreq2-devel libcurl-devel protobuf-devel openssl-devel libpng-devel java-1.8.0-openjdk java-1.8.0-openjdk-devel && \
  yum-builddep -y R-devel && \
  yum clean all

# R version
# https://github.com/rstudio/r-docker/blob/master/3.2/centos7/Dockerfile
# Can't use because RPM R not build as shared library which rapache needs
# ENV R_VERSION 3.2.3
# RUN curl -O https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm && \
#     yum -y install ./R-${R_VERSION}-1-1.x86_64.rpm && \
#     ln -s /opt/R/${R_VERSION}/bin/R /usr/bin/R && \
#     ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/bin/Rscript && \
#     ln -s /opt/R/${R_VERSION}/lib/R /usr/lib/R && \
#     rm R-${R_VERSION}-1-1.x86_64.rpm && \
#     yum clean all

ENV R_VERSION 3.6.3
RUN curl -O https://cran.rstudio.com/src/base/R-3/R-${R_VERSION}.tar.gz && \
    tar -xzvf R-${R_VERSION}.tar.gz && \
    cd R-${R_VERSION} && \
    ./configure \
        --prefix=/opt/R/${R_VERSION} \
        --enable-memory-profiling \
        --enable-R-shlib \
        --with-blas \
        --with-lapack && \
    make && \
    make install

RUN ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R
RUN ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript

ENV JAVA_HOME /usr/lib/jvm/java-1.8.0
ENV LD_LIBRARY_PATH $JAVA_HOME/jre/lib/amd64:$JAVA_HOME/jre/lib/amd64/server/
RUN echo "$JAVA_HOME/jre/lib/amd64/server/" > /etc/ld.so.conf.d/rApache_rJava.conf && \
    /sbin/ldconfig && \
    R CMD javareconf

USER builder

RUN \
  mkdir -p ~/rpmbuild/SOURCES && \
  mkdir -p ~/rpmbuild/SPECS

RUN \
  cd ~ && \
  wget https://github.com/jeffreyhorner/rapache/archive/v1.2.9.tar.gz -O rapache-1.2.9.tar.gz && \
  tar xzvf rapache-1.2.9.tar.gz && \
  cd rapache-1.2.9 && \
  ./configure \
    --with-R=/usr/local/bin/R\
    --with-apreq2-config=/usr/bin/apreq2-config && \
  make

USER root

RUN \
  yum install -y MTA mod_ssl /usr/sbin/semanage && \
  cd /home/builder/rapache-1.2.9 && \
  make install

  
RUN \
  chkconfig httpd off \
  userdel builder

RUN yum -y localinstall https://download.postgresql.org/pub/repos/yum/9.4/redhat/rhel-7-x86_64/pgdg-centos94-9.4-3.noarch.rpm  && \
  yum install -y postgresql94-devel curl-devel libxml2-devel

# RUN yum -y localinstall https://download.postgresql.org/pub/repos/yum/12/redhat/rhel-7-x86_64/pgdg-redhat-repo-42.0-11.noarch.rpm  && \
#   yum install -y pgdg-redhat-repo-42.0-11.noarch.rpm curl-devel libxml2-devel


RUN	useradd -u 1000 -ms /bin/bash runner
ENV ACAS_HOME /home/runner/build
RUN echo $ACAS_HOME && mkdir $ACAS_HOME

WORKDIR $ACAS_HOME

ENV R_LIBS $ACAS_HOME/r_libs
RUN mkdir $ACAS_HOME/r_libs
ENV PATH /usr/pgsql-9.4/bin:$PATH

COPY	DESCRIPTION $ACAS_HOME
COPY	R/installation.R $ACAS_HOME
RUN chown -R runner:runner $ACAS_HOME
RUN yum install -y nlopt nlopt-devel
USER runner
RUN  Rscript -e "source('installation.R');installDeps(sections = c('Imports', 'Suggests'));" && \
     rm DESCRIPTION installation.R

COPY	--chown=runner:runner . /home/runner/racas
RUN  export R_LIBS=/home/runner/build/r_libs && R CMD INSTALL --no-multiarch --with-keep.source /home/runner/racas
EXPOSE 1080

RUN cp -R /home/runner/racas/inst/bin /home/runner/bin

CMD ["bin/acas.sh", "run", "rservices"]