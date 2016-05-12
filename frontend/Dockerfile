FROM ulexus/meteor
# FROM phusion/passenger-nodejs
# FROM golden/meteor-dev
# FROM node

COPY phantomjs-1.9.8-linux-x86_64.tar.bz2 /tmp/

RUN apt-get update
#RUN apt-get install -y build-essential chrpath libssl-dev libxft-dev
RUN apt-get install -y libfreetype6 libfreetype6-dev
RUN apt-get install -y libfontconfig1 libfontconfig1-dev

RUN apt-get install -y bzip2
RUN tar xvjf /tmp/phantomjs-1.9.8-linux-x86_64.tar.bz2
RUN mv phantomjs-1.9.8-linux-x86_64 /usr/local/share
RUN ln -sf /usr/local/share/phantomjs-1.9.8-linux-x86_64/bin/phantomjs /usr/local/bin

# RUN apt-get update
#RUN apt-get -y dist-upgrade
#RUN apt-get install -y curl
RUN curl https://install.meteor.com/ | sh
WORKDIR /opt/application


