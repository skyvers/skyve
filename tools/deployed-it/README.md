# Deployed Integration Tests

Skyve's deployed-integration harness builds a classified test-overlay WAR, starts an isolated
checksum-approved WildFly installation, and selects one standard Failsafe `*IT` class. The
ordinary release WAR and default Maven build never contain the overlay transport or test code.

## Validate the Harness Shape

Validation-only mode checks generic test selection and prints a redacted Maven command without
requiring WildFly, a runtime pin, a browser, configuration, or credentials:

```bash
SKYVE_DEPLOYED_IT_TEST=example.ReusableHarnessIT tools/deployed-it/run.sh --validate-only
```

The selected value must be a Java class name ending in `IT`. It is passed as one array argument to
Maven's standard `-Dit.test` property; the script does not evaluate shell text.

## Runtime Approval and Preparation

The release owner must approve `runtime.properties` before any deployed runtime is prepared. That
file is deliberately absent while approval is pending and must contain exactly these reviewed
values:

```properties
wildfly.version=<approved version>
wildfly.url=<approved .zip or .tar.gz distribution URL>
wildfly.sha256=<approved 64-character distribution SHA-256>
mojarra.version=<verified bundled implementation version>
```

Neither scripts nor GitHub Actions infer a latest version. Updating any value requires a runtime
support and harness review. Once approved, prepare an installation at an explicit empty target:

```bash
SKYVE_DEPLOYED_IT_RUNTIME_DIR=/absolute/path/to/wildfly \
tools/deployed-it/prepare-runtime.sh
```

An existing archive may be supplied through `SKYVE_DEPLOYED_IT_RUNTIME_ARCHIVE`; its checksum and
bundled Mojarra version are still verified. The supplied installation is never modified by a test
run.

## Isolated H2 Configuration

`config/skyve.json.template` and `config/skyve-h2-ds.xml.template` are non-secret file-backed H2
templates. Materialise them under temporary state with per-run credentials:

```bash
SKYVE_DEPLOYED_IT_CONFIG_DIR=/absolute/tmp/config \
SKYVE_DEPLOYED_IT_CONTENT_DIR=/absolute/tmp/content \
SKYVE_DEPLOYED_IT_DATABASE_PATH=/absolute/tmp/database/skyve \
SKYVE_DEPLOYED_IT_USERNAME=deployed_it \
SKYVE_DEPLOYED_IT_PASSWORD='<generated protected value>' \
SKYVE_DEPLOYED_IT_BASE_URL=http://127.0.0.1:8080/skyve/ \
tools/deployed-it/prepare-h2-config.sh
```

The generated files are mode-restricted and must remain outside the repository. The base URL is
also written into Skyve's canonical server URL so login redirects cannot cross into another local
runtime. When a suite
requires an existing user instead, supply `SKYVE_DEPLOYED_IT_USERNAME` and
`SKYVE_DEPLOYED_IT_PASSWORD` through a protected environment or an equivalent Maven-settings
server entry. Commands, logs, and results must never contain those values.

## Run One Suite

```bash
SKYVE_DEPLOYED_IT_TEST=example.ReusableHarnessIT \
SKYVE_DEPLOYED_IT_RUNTIME_DIR=/absolute/path/to/wildfly \
SKYVE_DEPLOYED_IT_CONFIG_DIR=/absolute/path/to/isolated-config \
SKYVE_DEPLOYED_IT_BASE_URL=http://127.0.0.1:8080/skyve/ \
tools/deployed-it/run.sh
```

`run.sh` copies only the runtime's `standalone` base into a temporary directory, deploys the
isolated datasource and passes the external Skyve JSON path explicitly, builds
`skyve-war/target/skyve-deployed-it.war`, deploys it, waits
for readiness, and invokes:

```text
mvn -pl skyve-war -am -Pdeployed-it -DskipIntegrationTests=false -DskipUnitTests=true -Dfailsafe.failIfNoSpecifiedTests=false -Dit.test=<selected-class> verify
```

A trap stops the server and removes temporary state on success, failure, or interruption.

## Overlay and Java Extension Contract

The Maven profile adds `skyve-war/src/deployed-it/java` only as a test source and copies only
`util/deployed/transport/**` plus `src/deployed-it/webapp` into the classified WAR. The common
transport provides:

- an authenticated probe at `/deployed-it/probe`;
- a server-issued token bound to the authenticated HTTP session and correlation id;
- a recorder bounded by correlation count and events per correlation; and
- ordered generic event documents with no feature vocabulary.

A new suite lives in the standard `skyve-war/src/test/java` tree and ends in `IT`. It should reuse
`util.deployed` for configuration, browser login/logout, view-state extraction, normal and
PrimeFaces Ajax postbacks, probe correlation, and redacted diagnostics. Feature-specific event
names, hooks, fixtures, and assertions belong to that suite's overlay adapter, never the generic
transport, Maven profile, scripts, or workflow.

SAIL is a future consumer of this contract. A future SAIL `*IT` should reuse the deployment,
browser, authentication, diagnostics, and CI lifecycle while loading approved SAIL definitions
from repository or classpath resources. Migrating existing SAIL suites is outside M2.

## Results and Diagnostics

Every run writes `skyve-war/target/deployed-it/<test-class>/results.md`. Contract version 1 records
the redacted command shape, selected test, externally supplied commit identity, committed runtime
pin, verified checksum and Mojarra version, Java/browser/driver versions, overlay checksum, base
URL shape, and result. The same directory holds WildFly console/server logs and browser
screenshots or page diagnostics. Failsafe XML remains in `skyve-war/target/failsafe-reports`.

The reusable `.github/workflows/deployed-it.yml` accepts a required test-class input, prepares all
mutable state under the runner temporary directory, and calls these same scripts. It always
publishes Failsafe XML, `results.md`, WildFly logs, and browser diagnostics. The workflow needs no
repository credential when the H2/bootstrap path is supported; suites that require protected
credentials must be restricted to trusted events and cannot be required fork-pull-request checks.
