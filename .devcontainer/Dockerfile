FROM mcr.microsoft.com/devcontainers/base:debian
MAINTAINER Trifon Trifonov <triffon@fmi.uni-sofia.bg>

# install basic packages
RUN apt-get update && apt-get install -y locales make racket git xvfb

# install minlog
RUN git clone https://github.com/triffon/minlog.git /usr/src/minlog
WORKDIR /usr/src/minlog
RUN make

# set locale to en_US to avoid localization problems
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# workaround for Racket Language Server issue
# https://github.com/Eugleo/magic-racket/issues/51
# based on https://github.com/SFurnace/racket-devcontainer/
ENV DISPLAY=:0
COPY xvfb-racket /usr/local/bin
RUN chmod +x /usr/local/bin/xvfb-racket

# install Racket language server
RUN raco pkg install --scope installation --auto racket-langserver
