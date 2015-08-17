FROM ubuntu:latest

RUN apt-get update
RUN apt-get install -y python2.7 python-pip
RUN apt-get install -y nodejs npm
RUN sudo ln -s "$(which nodejs)" /usr/bin/node
RUN apt-get install -y git
RUN npm install -g elm

ADD . lemur
WORKDIR lemur
RUN make deps && make

EXPOSE 3000

CMD ["node", "server.js"]
