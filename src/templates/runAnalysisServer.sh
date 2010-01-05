#!/bin/bash
nohup $JAVA_HOME/bin/java -Xmx512m -Xms128m -classpath ./:../lib/concurrent.jar:../lib/jbossall-client.jar:../lib/log4j.jar:../lib/Rserve.jar:../lib/REngine.jar:../lib/caintegrator-analysis-commons.jar:../lib/caintegrator-analysis-server.jar gov.nih.nci.caintegrator.analysis.server.AnalysisServer  analysisServer.properties &
