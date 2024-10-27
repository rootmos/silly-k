FROM alpine:3.20 AS chezscheme

WORKDIR /workdir

ADD --checksum=sha256:f5827682fa259c47975ffe078785fb561e4a5c54f764331ef66c32132843685d \
        https://github.com/cisco/ChezScheme/releases/download/v9.6.4/csv9.6.4.tar.gz \
        csv.tar.gz

RUN apk update && apk add \
        libarchive-tools patch \
        build-base ncurses-dev libx11-dev util-linux-dev

RUN bsdtar -xf csv.tar.gz --strip-components=1

COPY srfi-0.patch .
RUN patch -p1 < srfi-0.patch

RUN ./configure --temproot=/pkg
RUN make install


FROM alpine:3.20

RUN apk update && apk add \
        ncurses util-linux \
        opam shadow \
        make gcc musl-dev \
        gmp-dev pkgconf

COPY --from=chezscheme /pkg /

RUN useradd -m opam
WORKDIR /silly-k
RUN chown opam:opam /silly-k
USER opam

RUN opam init --bare --no-setup --disable-sandboxing
RUN opam switch create 5.2.0
RUN opam install --yes malfunction.0.6

COPY --chown=opam lalr-scm lalr-scm
COPY --chown=opam nanopass-framework-scheme nanopass-framework-scheme

COPY silly-k.scm repl.scm tests.scm Makefile .

RUN make precompile

ENTRYPOINT ["opam", "exec", "--", "make", "--quiet"]
