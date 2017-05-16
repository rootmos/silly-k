FROM rootmos/silly-k-base

RUN mkdir silly-k
WORKDIR silly-k

ADD .git .git
ADD .gitmodules .
RUN sudo chown opam -R .
RUN git submodule init
RUN git submodule update

ADD silly-k.scm .
ADD silly-k silly-k
ADD repl.scm .
ADD repl .
ADD tests.scm .
ADD Makefile .
RUN sudo chown opam -R .

RUN make precompile

ENTRYPOINT ["opam", "config", "exec", "--", "make", "--quiet"]
