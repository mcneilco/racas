FROM mcneilco/acas-r-repo:1.13.7

ENV LANG en_US.UTF-8
COPY --chown=runner:runner . /home/runner/racas
RUN  export R_LIBS=/home/runner/build/r_libs && R CMD INSTALL --no-multiarch --with-keep.source /home/runner/racas
EXPOSE 1080

COPY --chown=runner:runner ./inst/bin /home/runner/build/bin
COPY --chown=runner:runner ./inst/PrepareConfigFiles.coffee /home/runner/build/src/javascripts/BuildUtilities/PrepareConfigFiles.coffee
CMD ["bin/acas.sh", "run", "rservices"]