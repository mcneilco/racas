FROM mcneilco/acas-r-repo:2023.1.0

# NODE
USER root
ENV NPM_CONFIG_LOGLEVEL warn
ENV NODE_VERSION 18.x
RUN curl -fsSL https://rpm.nodesource.com/setup_$NODE_VERSION | bash - && \
  dnf install -y nodejs && \
  npm install -g coffeescript@2.5.1 properties@1.2.1 underscore@1.12.0 underscore-deep-extend@1.1.5 properties-parser@0.3.1 flat@5.0.2 glob@7.1.6
USER runner
ENV NODE_PATH /usr/lib/node_modules

ENV LANG en_US.UTF-8
ENV LC_ALL C.UTF-8
COPY --chown=runner:runner . /home/runner/racas
RUN  export R_LIBS=/home/runner/build/r_libs && R CMD INSTALL --no-multiarch --with-keep.source /home/runner/racas
EXPOSE 1080

COPY --chown=runner:runner ./inst/bin /home/runner/build/bin
COPY --chown=runner:runner ./inst/PrepareConfigFiles.coffee /home/runner/build/src/javascripts/BuildUtilities/PrepareConfigFiles.coffee
CMD ["bin/acas.sh", "run", "rservices"]