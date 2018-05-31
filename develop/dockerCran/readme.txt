Only the tags and latest are built automatically on dockerhub.
This dockerfile installs REddyProc from CRAN

Need to build this manually by
docker build -t bgctw/reddyproc_cran .
docker login --username=bgctw
   pseudo docker.com
docker push bgctw/reddyproc_cran

