FROM rootmos/chezscheme:v9.4.1-with-cond-expand

RUN mkdir /silly-k
WORKDIR /silly-k

RUN apk update && apk add opam git m4 build-base gmp-dev perl

RUN opam init -a -y --comp=4.03.0+flambda
RUN sed -i '/termux/d' /root/.opam/repo/default/packages/ocamlfind/ocamlfind.1.7.1/opam
RUN opam update && opam install conf-m4 jbuilder omd
RUN opam pin add malfunction git://github.com/stedolan/malfunction.git

ADD .git .git
ADD .gitmodules .
RUN git submodule update

ADD silly-k.scm .
ADD silly-k silly-k
ADD repl.scm .
ADD repl .
ADD tests.scm .
ADD Makefile .

RUN make precompile

ENTRYPOINT ["opam", "config", "exec", "make"]
