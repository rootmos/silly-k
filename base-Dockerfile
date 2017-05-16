FROM rootmos/chezscheme:v9.4.1-with-cond-expand

RUN apk add sudo && \
        adduser -S opam && \
        echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam && \
        chmod 440 /etc/sudoers.d/opam && \
        chown root:root /etc/sudoers.d/opam
USER opam
WORKDIR /home/opam
RUN sudo apk update && sudo apk add opam git m4 build-base gmp gmp-dev perl && \
        opam init -a -y --comp=4.03.0+flambda && \
        sed -i '/termux/d' /home/opam/.opam/repo/default/packages/ocamlfind/ocamlfind.1.7.1/opam && \
        opam update && opam install conf-m4 jbuilder omd && \
        opam pin add malfunction git://github.com/stedolan/malfunction.git && \
        sudo apk del m4 gmp-dev perl

ENTRYPOINT ["opam", "config", "exec", "--"]
