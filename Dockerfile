FROM uribo/ramora:latest

RUN set -x && \
  apt-get update && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ARG GITHUB_PAT

RUN set -x && \
  echo "GITHUB_PAT=$GITHUB_PAT" >> /usr/local/lib/R/etc/Renviron

RUN set -x && \
  installGithub.r \
    uribo/zipangu && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
