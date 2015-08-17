FROM node:latest

RUN npm install -g elm

ADD . lemur
WORKDIR lemur
RUN make deps && make

EXPOSE 3000

CMD ["node", "server.js"]
