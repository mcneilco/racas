ARG NODE_VERSION=22.23.4
ARG REPO_IMAGE_VERSION=master

# Node stage: install the global modules PrepareConfigFiles.coffee needs at
# container startup. Only the node binary and these modules are copied into
# the final image (npm/corepack are build-time tooling and are left behind).
FROM node:${NODE_VERSION}-slim AS node
ENV NPM_CONFIG_LOGLEVEL=warn
RUN npm install -g \
    coffeescript@2.5.1 \
    properties@1.2.1 \
    underscore@1.12.0 \
    underscore-deep-extend@1.1.5 \
    properties-parser@0.3.1 \
    flat@5.0.2 \
    glob@7.1.6 && \
    rm -rf /usr/local/lib/node_modules/npm /usr/local/lib/node_modules/corepack

FROM mcneilco/acas-r-repo:${REPO_IMAGE_VERSION}

LABEL maintainer="acas-developers-group@schrodinger.com"
LABEL org.opencontainers.image.source="https://github.com/mcneilco/racas"
LABEL org.opencontainers.image.description="ACAS R services (racas, rApache)"

USER root
# Apply OS security errata published since the acas-r-repo base was built.
# PKG_REFRESH (e.g. a date) is passed by CI so this layer is not served stale
# from the build cache.
ARG PKG_REFRESH=unset
# The upgrade can move Java to a new versioned directory, so (re)point the loader at the stable
# /usr/lib/jvm/jre symlink; a base built before this change has the old versioned path baked in.
RUN echo "package refresh: ${PKG_REFRESH}" && \
    microdnf upgrade -y && \
    microdnf clean all && \
    ls -d /usr/lib/jvm/jre/lib/*/server > /etc/ld.so.conf.d/java.conf && \
    ldconfig

# NODE (runtime: PrepareConfigFiles.coffee on startup, confToView.R)
COPY --from=node /usr/local/bin/node /usr/local/bin/node
COPY --from=node /usr/local/lib/node_modules /usr/local/lib/node_modules
RUN ln -s ../lib/node_modules/coffeescript/bin/coffee /usr/local/bin/coffee
USER runner
ENV NODE_PATH=/usr/local/lib/node_modules

ENV LANG=en_US.UTF-8
ENV LC_ALL=C.UTF-8
COPY --chown=runner:runner . /home/runner/racas
RUN  export R_LIBS=/home/runner/build/r_libs && R CMD INSTALL --no-multiarch --with-keep.source /home/runner/racas
# Fail the build, instead of shipping a broken image, if Excel reading (XLConnect -> rJava -> JVM) does not work
RUN  export R_LIBS=/home/runner/build/r_libs && Rscript -e 'library(XLConnect); cat("XLConnect + rJava ok, java", rJava::.jcall("java/lang/System", "S", "getProperty", "java.version"), "\n")'
EXPOSE 1080

COPY --chown=runner:runner ./inst/bin /home/runner/build/bin
COPY --chown=runner:runner ./inst/PrepareConfigFiles.coffee /home/runner/build/src/javascripts/BuildUtilities/PrepareConfigFiles.coffee
CMD ["bin/acas.sh", "run", "rservices"]
