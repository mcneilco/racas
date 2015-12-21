FROM mcneilco/centos-node-r-repo:compound-inventory

USER	root

COPY	. /home/runner/racas
RUN  export R_LIBS=/home/runner/build/r_libs && R CMD INSTALL --no-multiarch --with-keep.source /home/runner/racas
RUN  rm -rf /home/runner/racas
