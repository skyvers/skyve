### background
- Tried making a shaded jar with Undertow, RestEast and Weld.
- All that worked fine except there is no way to deploy a web app easily with weld.
- It only has a programmatic builder interface.
- Jetty and Tomcat have API where you can point to an existing war and deploy it.

- Decided on tomcat because Weld had existing example of integrating with it.

- Couldn't find definitive example of tomcat and resteasy or tomcat and jersey.
- Looks like there is a listener for any web container in resteasy though.

### Used this as my starting point with undertow resteast and weld
- https://wiki.jarylchng.com/en/programming/java/minimal-undertow-resteasy-and-weld-cdi-setup

### Then looking at tomcat, resteasy and weld
- https://www.codejava.net/servers/tomcat/how-to-embed-tomcat-server-into-java-web-applications
- https://docs.jboss.org/weld/reference/latest/en-US/html/environments.html
- https://itnext.io/building-jakarta-ee-9-web-application-with-servlet-containers-b3acc50c8464
- https://github.com/MehrabRahman/embedded-tomcat-rest-example/tree/master
- https://docs.jboss.org/resteasy/docs/3.0.4.Final/userguide/html/Installation_Configuration.html#d4e111 - heading is "Standalone Resteasy in Servlet 3.0 Containers"
- https://zetcode.com/jaxrs/resteasytomcatcdi/#google_vignette - RestEasy/Tiomcat/Weld



### Jetty links
- https://stackoverflow.com/questions/20786661/configure-embedded-jetty-with-web-xml
- https://github.com/jetty-project/embedded-jetty-weld/tree/jetty-10.0.x

### Cargo links (Probs not)
- https://codehaus-cargo.github.io/cargo/Quick+start.html
- https://www.baeldung.com/integration-testing-with-the-maven-cargo-plugin
