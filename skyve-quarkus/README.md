# description

Tried using VSCode with quarkus plugin.

# problem so far

- Could not get jndi to work with hibernate and cannot inject the agroal data source because hibernate statically initialises.
    - Could try injecting @ApplicationScoped bean with @PostConstruct (is thread-safe) to configure hibernate.
    - If could inject DataSource then  would use
        - StandardServiceRegistryBuilder ssrb = ......
        - ssrb.applySetting(AvailableSettings.DATASOURCE, ds);
- So set to use without JNDI with H2 (driver/url/user/password) and found that UtilImpl.inject() did not work.
    - Caused by: java.lang.UnsupportedOperationException
        at io.quarkus.arc.impl.BeanManagerImpl.createAnnotatedType(BeanManagerImpl.java:241)
        at org.skyve.impl.util.UtilImpl.inject(UtilImpl.java:526)
        at org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence.postLoad(AbstractHibernatePersistence.java:2284)
    - Might need to Create a CDI Producer class in gen domain and use it in DocumentImpl.
    - But can't inject dependencies into hibernate entity extensions. Both JPA and CDI use proxies and will clash. Can inject in listeners.
    - Maybe Bizlets should be request scoped CDI beans (with state allowed).
    - Can I class load and call a method in quarkus?
        - Yes, https://javadoc.io/doc/io.quarkus/quarkus-core/latest/io/quarkus/runtime/annotations/RegisterForReflection.html

    

# code-with-quarkus

This project uses Quarkus, the Supersonic Subatomic Java Framework.

If you want to learn more about Quarkus, please visit its website: https://quarkus.io/ .

## Running the application in dev mode

You can run your application in dev mode that enables live coding using:
```shell script
./mvnw compile quarkus:dev
```

> **_NOTE:_**  Quarkus now ships with a Dev UI, which is available in dev mode only at http://localhost:8080/q/dev/.

## Packaging and running the application

The application can be packaged using:
```shell script
./mvnw package
```
It produces the `quarkus-run.jar` file in the `target/quarkus-app/` directory.
Be aware that it’s not an _über-jar_ as the dependencies are copied into the `target/quarkus-app/lib/` directory.

The application is now runnable using `java -jar target/quarkus-app/quarkus-run.jar`.

If you want to build an _über-jar_, execute the following command:
```shell script
./mvnw package -Dquarkus.package.type=uber-jar
```

The application, packaged as an _über-jar_, is now runnable using `java -jar target/*-runner.jar`.

## Creating a native executable

You can create a native executable using: 
```shell script
./mvnw package -Dnative
```

Or, if you don't have GraalVM installed, you can run the native executable build in a container using: 
```shell script
./mvnw package -Dnative -Dquarkus.native.container-build=true
```

You can then execute your native executable with: `./target/code-with-quarkus-1.0.0-SNAPSHOT-runner`

If you want to learn more about building native executables, please consult https://quarkus.io/guides/maven-tooling.

## Provided Code

### RESTEasy Reactive

Easily start your Reactive RESTful Web Services

[Related guide section...](https://quarkus.io/guides/getting-started-reactive#reactive-jax-rs-resources)
