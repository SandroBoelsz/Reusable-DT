# Default base image
FROM ubuntu:22.04

# Install required tools
RUN apt-get update && \
    apt-get install -y openjdk-17-jre wget && \
    apt-get clean

# SET JAVA_HOME
ENV JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
ENV PATH="${JAVA_HOME}/bin:${PATH}"

# Install NetLogo 6.3.0
RUN wget https://ccl.northwestern.edu/netlogo/6.3.0/NetLogo-6.3.0-64.tgz && \
    tar -xzf NetLogo-6.3.0-64.tgz && \
    mv NetLogo\ 6.3.0 /opt/netlogo-6.3.0 && \
    ln -s /opt/netlogo-6.3.0/netlogo-headless.sh /usr/local/bin/netlogo-headless && \
    rm NetLogo-6.3.0-64.tgz

ENV NETLOGO_HOME=/opt/netlogo-6.3.0
ENV NETLOGO_JAR=${NETLOGO_HOME}/lib/app/netlogo-6.3.0.jar
ENV PATH="${NETLOGO_HOME}:${PATH}"
