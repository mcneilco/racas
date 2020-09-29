ARG ACAS_IMAGE=mcneilco/acas-oss:latest
FROM ${ACAS_IMAGE} AS acas-src

FROM mcneilco/centos-r-repo:XLConnect
COPY	. /home/runner/racas
USER runner
RUN  export R_LIBS=/home/runner/build/r_libs && R CMD INSTALL --no-multiarch --with-keep.source /home/runner/racas
EXPOSE 1080

COPY --from=acas-src --chown=runner:runner /home/runner/build/bin /home/runner/build/bin
COPY --from=acas-src --chown=runner:runner /home/runner/build/conf/compiled /home/runner/build/conf/compiled
COPY --from=acas-src --chown=runner:runner /home/runner/build/src/r /home/runner/build/src/r

CMD ["bin/acas.sh", "run", "rservices"]