FROM rootmos/chezscheme:v9.4.1-with-cond-expand

RUN mkdir /silly-k
WORKDIR /silly-k

RUN apt-get update
RUN apt-get install -y opam rlwrap

RUN opam init
RUN opam switch 4.03.0+flambda
RUN opam update
RUN apt-get install -y m4 libgmp3-dev
RUN opam install jbuilder omd
RUN opam pin add malfunction git://github.com/stedolan/malfunction.git

ADD . .
RUN git submodule init

ENTRYPOINT ["opam", "config", "exec", "make"]
