#
# Build stage
#
FROM maven:3.6.3-openjdk-17-slim AS build
COPY contingent/application /opt/src
COPY contingent/lib/ /opt/src/libs
COPY .git /opt/src
RUN apt-get update && apt-get install -y git
RUN mvn install:install-file -Dfile=/opt/src/libs/client-lib-4.5.1.jar -DgroupId=ru.mos.emias.esu -DartifactId=client-lib -Dversion=4.5.1 -Dpackaging=jar
RUN mvn -f /opt/src/pom.xml validate clean install -DskipTests=true -e

FROM docker.artifactory.emias.mos.ru/emiasos-openjdk:17.0.4.1 AS contingent2-area
ARG JAR_FILE=contingent-area/target/area-*.jar
COPY --from=build /opt/src/${JAR_FILE} /opt/area.jar
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -jar /opt/area.jar"]

FROM docker.artifactory.emias.mos.ru/emiasos-openjdk:17.0.4.1 AS contingent2-attachment
ARG JAR_FILE=contingent-attachment-application/target/attachment-*.jar
COPY --from=build /opt/src/${JAR_FILE} /opt/attachment.jar
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -jar /opt/attachment.jar"]

FROM docker.artifactory.emias.mos.ru/emiasos-openjdk:17.0.4.1 AS contingent2-nsi
ARG JAR_FILE=contingent-application-nsi/target/nsi-*.jar
COPY --from=build /opt/src/${JAR_FILE} /opt/nsi.jar
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -jar /opt/nsi.jar"]

FROM docker.artifactory.emias.mos.ru/emiasos-openjdk:17.0.4.1 AS contingent2-sysop
ARG JAR_FILE=contingent-sysop-application/target/sysop-*.jar
COPY --from=build /opt/src/${JAR_FILE} /opt/sysop.jar
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -jar /opt/sysop.jar"]