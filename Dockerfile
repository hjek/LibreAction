FROM swipl
COPY ./ /act-now/
EXPOSE 8080/tcp
EXPOSE 8080/udp
WORKDIR /act-now/
ENTRYPOINT ["swipl", "-f", "/act-now/act-now.pl", "-q", "--stack_limit=4m", "-g", "main"]
