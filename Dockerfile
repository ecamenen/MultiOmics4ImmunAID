# Author: Etienne CAMENEN
# Date: 2022
# Contact: etienne.camenen@gmail.com

FROM rocker/rstudio:4.0

MAINTAINER Etienne CAMENEN (etienne.camenen@gmail.com)

ENV _R_CHECK_FORCE_SUGGESTS_ FALSE
ENV PKGS cmake git libcurl4-openssl-dev libglu1-mesa liblapack-dev libssl-dev libxml2-dev qpdf texlive-fonts-recommended texlive-latex-extra texlive-latex-recommended
ARG TOOL_NAME
ARG TOOL_VERSION
RUN apt-get update --allow-releaseinfo-change -qq && \
    apt-get install -y ${PKGS}
RUN apt-get install -y --no-install-recommends libglpk-dev libxt6
ENV RPKGS attachment BiocManager config covr cowplot DataExplorer data.tree devtools dlookr ggforce ggpubr ggstatsplot globals golem gplots janitor knitr lintr lubridate markdown moments naniar networkD3 openxlsx pkgload plotly psych rayshader RColorBrewer report reshape2 rlist rmarkdown rsconnect rstatix see sjPlot skimr styler tidylog tidyverse venn VIM viridis
RUN Rscript -e "install.packages(commandArgs(TRUE))" ${RPKGS}
RUN R -e "devtools::install_github('ecamenen/"${TOOL_NAME}"', ref = '"${TOOL_VERSION}"')"
RUN Rscript -e "BiocManager::install('BiocCheck')"
RUN Rscript -e 'devtools::install_github("moldach/vapoRwave")'
# RUN Rscript -e "install.packages(commandArgs(TRUE))" datawizard
# RUN R -e "install.packages('bayestestR', repos = 'https://easystats.r-universe.dev')"
# RUN R -e "library('bayestestR')"
# RUN R -e "library('datawizard')"
RUN R -e "library('sjPlot')"
RUN R -e "library('rayshader')"
RUN Rscript -e 'devtools::install_github("rgcca-factory/RGCCA")'
RUN apt-get purge -y git g++ && \
	apt-get autoremove --purge -y && \
	apt-get clean && \
	rm -rf /var/lib/{cache,log}/ /tmp/* /var/tmp/*
COPY . /home/rstudio
