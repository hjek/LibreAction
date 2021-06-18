#!/usr/bin/bash
docker build -t swipl .
docker run -it -p 8080:8080 swipl

