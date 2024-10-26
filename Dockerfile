FROM alpine:3.20 AS chezscheme

WORKDIR /workdir

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

RUN apk add sudo && \
        adduser -S opam && \
        echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam && \
        chmod 440 /etc/sudoers.d/opam && \
        chown root:root /etc/sudoers.d/opam
USER opam
WORKDIR /home/opam

RUN opam init --no-setup --disable-sandboxing
RUN opam install --yes malfunction

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

ENTRYPOINT ["opam", "exec", "--", "make", "--quiet"]
