FROM alpine:3.20 AS chezscheme

WORKDIR /workdir

#ADD --checksum=sha256:90bddd47bae6bc97d0daacbace9606a955fd32b98ff56c38bbad4d6903e182e5 \
        #https://github.com/cisco/ChezScheme/releases/download/v9.5/csv9.5.tar.gz \
        #csv.tar.gz

ADD --checksum=sha256:f5827682fa259c47975ffe078785fb561e4a5c54f764331ef66c32132843685d \
        https://github.com/cisco/ChezScheme/releases/download/v9.6.4/csv9.6.4.tar.gz \
        csv.tar.gz

RUN apk update
RUN apk add libarchive-tools patch

RUN bsdtar -xvzf csv.tar.gz --strip-components=1

COPY srfi-0.patch .
RUN patch -p1 < srfi-0.patch

RUN apk add build-base ncurses-dev libx11-dev util-linux-dev

RUN ./configure
RUN make install

#FROM rootmos/chezscheme:v9.4.1-with-cond-expand
#FROM builder

RUN apk add sudo && \
        adduser -S opam && \
        echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam && \
        chmod 440 /etc/sudoers.d/opam && \
        chown root:root /etc/sudoers.d/opam
USER opam
WORKDIR /home/opam

RUN sudo apk update && sudo apk add opam git m4 build-base gmp gmp-dev perl 
RUN opam init --bare --no-setup --disable-sandboxing
RUN opam switch create 4.04.0
#RUN opam init --auto-setup ---y --comp=4.03.0+flambda
#RUN sed -i '/termux/d' /home/opam/.opam/repo/default/packages/ocamlfind/ocamlfind.1.7.1/opam
#RUN opam update
#RUN opam install --yes conf-m4 jbuilder omd
#RUN opam pin add malfunction git://github.com/stedolan/malfunction.git
#RUN opam pin add malfunction https://github.com/stedolan/malfunction
RUN opam install --yes malfunction
RUN sudo apk del m4 gmp-dev perl

RUN mkdir silly-k
WORKDIR silly-k

ADD .git .git
ADD .gitmodules .
RUN sudo chown opam -R .
RUN git submodule init
RUN git submodule update

ADD silly-k.scm .
ADD repl.scm .
ADD tests.scm .
ADD Makefile .
RUN sudo chown opam -R .

RUN make precompile

ENTRYPOINT ["opam", "config", "exec", "--", "make", "--quiet"]
