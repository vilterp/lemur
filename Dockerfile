FROM node:latest

RUN npm install -g elm@0.16.0

ADD . lemur
WORKDIR lemur
RUN make clean && make deps && make

EXPOSE 3000

CMD ["node", "server.js"]
