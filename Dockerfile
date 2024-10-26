FROM alpine:3.20 AS chezscheme

WORKDIR /workdir

ADD --checksum=sha256:f5827682fa259c47975ffe078785fb561e4a5c54f764331ef66c32132843685d \
        https://github.com/cisco/ChezScheme/releases/download/v9.6.4/csv9.6.4.tar.gz \
        csv.tar.gz

RUN apk update
RUN apk add libarchive-tools patch

RUN bsdtar -xf csv.tar.gz --strip-components=1

COPY srfi-0.patch .
RUN patch -p1 < srfi-0.patch

RUN apk add build-base ncurses-dev libx11-dev util-linux-dev

RUN ./configure --temproot=/pkg
RUN make install


FROM alpine:3.20

RUN apk add ncurses util-linux
COPY --from=chezscheme /pkg /
RUN scheme --version

RUN apk update && apk add opam shadow

RUN useradd -m opam
USER opam
WORKDIR /home/opam

RUN opam init --bare --no-setup --disable-sandboxing
#RUN opam switch create 4.03.0

USER root
RUN apk add gcc musl-dev
RUN apk add make
USER opam

RUN opam switch create 5.2.0

USER root
RUN apk add gmp-dev pkgconf
USER opam
RUN opam install --yes malfunction.0.6

RUN mkdir silly-k
WORKDIR silly-k

COPY --chown=opam lalr-scm lalr-scm
COPY --chown=opam nanopass-framework-scheme nanopass-framework-scheme

COPY silly-k.scm repl.scm tests.scm Makefile .

RUN make precompile

ENTRYPOINT ["opam", "exec", "--", "make", "--quiet"]
