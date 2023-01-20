#!/bin/bash

java -Dlogback.configurationFile=file:./logback.xml -Dconfig.file=./application.conf -classpath ./lib/*:target/scala-2.12/LocalGrader-assembly-1.0.jar se.kthx.grader.Main $@
