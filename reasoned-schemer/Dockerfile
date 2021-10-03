FROM racket/racket:7.7
RUN raco pkg install --deps force rackunit
RUN raco pkg install --deps force minikanren
WORKDIR /root/reasoned-schemer
COPY *.rkt ./
CMD ["racket", "tests.rkt"]
