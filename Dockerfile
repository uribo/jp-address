FROM uribo/ramora:latest

RUN set -x && \
  apt-get update && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ARG GITHUB_PAT

RUN set -x && \
  echo "GITHUB_PAT=$GITHUB_PAT" >> /usr/local/lib/R/etc/Renviron

RUN set -x && \
  install2.r --error --skipinstalled --repos 'http://mran.revolutionanalytics.com/snapshot/2019-12-02' \
    zipangu && \
  installGithub.r \
    uribo/kuniumi && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
