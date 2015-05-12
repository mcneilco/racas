FROM mcneilco/centos-node-r-repo

USER	root

COPY	. /home/runner/racas
RUN  ls /home/runner/racas
RUN  ls /home/runner
RUN  export R_LIBS=/home/runner/r_libs && R CMD INSTALL --no-multiarch --with-keep.source /home/runner/racas
RUN  rm -rf /home/runner/racas
