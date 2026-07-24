package util.deployed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

@SuppressWarnings("static-method")
class DeployedItStructureTest {
	private static final Path REPOSITORY = Path.of("..").toAbsolutePath().normalize();
	private static final Pattern ACTION = Pattern.compile("uses:\\s*[^@\\s]+@([^\\s#]+)");

	@Test
	void profileBuildsOnlyAClassifiedGenericOverlayUsingTheExistingFailsafeConvention() throws Exception {
		Document warPom = xml(REPOSITORY.resolve("skyve-war/pom.xml"));
		Element profile = profile(warPom, "deployed-it");
		String profileXml = profile.getTextContent();

		assertTrue(profileXml.contains("src/deployed-it/java"));
		assertTrue(profileXml.contains("src/deployed-it/webapp"));
		assertTrue(profileXml.contains("deployed-it-overlay-war"));
		assertEquals("deployed-it", descendant(profile, "classifier"));
		assertTrue(profileXml.contains("util/deployed/transport/**"));
		assertFalse(profileXml.contains("ThemeResolutionLifecycleIT"));
		assertFalse(profileXml.contains("wildfly.version"));
		assertFalse(profileXml.contains("password"));
		assertFalse(profileXml.contains("/Users/"));

		String parentPom = Files.readString(REPOSITORY.resolve("pom.xml"));
		assertTrue(parentPom.contains("<exclude>**/*IT</exclude>"));
		assertTrue(parentPom.contains("<include>**/*IT</include>"));
		assertTrue(parentPom.contains("<skipITs>${skipIntegrationTests}</skipITs>"));
	}

	@Test
	void ordinaryWarAndDependenciesHaveNoOverlaySurface() throws Exception {
		Document warPom = xml(REPOSITORY.resolve("skyve-war/pom.xml"));
		Element ordinaryBuild = firstDirectChild(warPom.getDocumentElement(), "build");
		assertNotNull(ordinaryBuild);
		String ordinaryBuildXml = ordinaryBuild.getTextContent();

		assertFalse(ordinaryBuildXml.contains("src/deployed-it"));
		assertFalse(ordinaryBuildXml.contains("util/deployed/transport"));
		NodeList dependencies = warPom.getElementsByTagName("dependency");
		for (int i = 0, size = dependencies.getLength(); i < size; i++) {
			Element dependency = (Element) dependencies.item(i);
			String artifactId = descendant(dependency, "artifactId");
			if ("selenide".equals(artifactId) || artifactId.contains("selenium") || artifactId.contains("webdriver")) {
				assertEquals("test", descendant(dependency, "scope"));
			}
		}
	}

	@Test
	void overlayTransportIsBoundedAuthenticatedAndFeatureNeutral() throws Exception {
		Path overlay = REPOSITORY.resolve("skyve-war/src/deployed-it");
		String recorder = Files.readString(overlay.resolve("java/util/deployed/transport/DeployedItEventRecorder.java"));
		String probe = Files.readString(overlay.resolve("java/util/deployed/transport/DeployedItProbeServlet.java"));
		String fixture = Files.readString(
				overlay.resolve("java/util/deployed/transport/theme/ThemeResolutionFixture.java"));

		assertTrue(recorder.contains("MAX_CORRELATIONS"));
		assertTrue(recorder.contains("MAX_EVENTS_PER_CORRELATION"));
		assertTrue(probe.contains("request.getUserPrincipal()"));
		assertTrue(probe.contains("SESSION_TOKEN_PREFIX"));
		assertTrue(probe.contains("/deployed-it/probe"));
		assertTrue(fixture.contains("SkyveFacesPhaseListener$FacesViewSelectionMarker"));
		assertTrue(fixture.contains("if (removedMarker == null)"));
		assertFalse((recorder + probe).toLowerCase().contains("theme"));
		assertTrue(Files.isRegularFile(overlay.resolve("webapp/WEB-INF/deployed-it-overlay.properties")));
	}

	@Test
	void scriptsUseArraySelectionRedactionIsolationAndTheGenericResultContract() throws Exception {
		Path tools = REPOSITORY.resolve("tools/deployed-it");
		String run = Files.readString(tools.resolve("run.sh"));
		String prepare = Files.readString(tools.resolve("prepare-runtime.sh"));
		String prepareH2 = Files.readString(tools.resolve("prepare-h2-config.sh"));
		String configTemplate = Files.readString(tools.resolve("config/skyve.json.template"));

		assertTrue(run.contains("--validate-only"));
		assertTrue(run.contains("maven_command=("));
		assertTrue(run.contains("\"${maven_command[@]}\""));
		assertTrue(run.contains("-Dit.test=$test_class"));
		assertTrue(run.contains("-pl skyve-war -am"));
		assertTrue(run.contains("-Dfailsafe.failIfNoSpecifiedTests=false"));
		assertTrue(run.contains("mktemp -d"));
		assertTrue(run.contains("target/deployed-it-webapp"));
		assertTrue(run.contains("cp -R \"$overlay_directory\" \"$server_base/deployments/skyve.war\""));
		assertTrue(run.contains("trap finish EXIT INT TERM"));
		assertTrue(run.contains("SKYVE_DEPLOYED_IT_BASE_URL already responds"));
		assertTrue(run.contains("credentials=<supplied-redacted>"));
		assertFalse(run.matches("(?s).*\\beval\\b.*"));
		assertFalse(run.contains("git "));
		assertTrue(prepare.contains("runtime.properties"));
		assertTrue(prepare.contains("wildfly.sha256"));
		assertTrue(prepare.contains("mojarra.version"));
		assertFalse(prepare.contains("latest"));
		assertTrue(prepareH2.contains("server_url=${base_url%/skyve/}"));
		assertTrue(prepareH2.contains("@SERVER_URL@"));
		assertTrue(configTemplate.contains("\"server\": \"@SERVER_URL@\""));

		for (String field : List.of("Contract version", "Command", "Commit", "Selected test", "WildFly version",
				"WildFly SHA-256", "Mojarra version", "Java", "Browser", "Driver", "Test-overlay SHA-256", "Result")) {
			assertTrue(run.contains(field), field);
		}
	}

	@Test
	void workflowIsReusablePinnedIsolatedAndAlwaysRetainsDiagnostics() throws Exception {
		String workflow = Files.readString(REPOSITORY.resolve(".github/workflows/deployed-it.yml"));
		String caller = Files.readString(REPOSITORY.resolve(".github/workflows/theme-resolution-deployed-it.yml"));

		assertTrue(workflow.contains("workflow_call:"));
		assertTrue(workflow.contains("workflow_dispatch:"));
		assertTrue(workflow.contains("test-class:"));
		assertTrue(workflow.contains("timeout-minutes:"));
		assertTrue(workflow.contains("java-version: '17'"));
		assertTrue(workflow.contains("${{ runner.temp }}"));
		assertTrue(workflow.contains("tools/deployed-it/prepare-runtime.sh"));
		assertTrue(workflow.contains("tools/deployed-it/run.sh"));
		assertTrue(workflow.contains("if: always()"));
		assertTrue(workflow.contains("**/target/failsafe-reports/TEST-*.xml"));
		assertTrue(workflow.contains("skyve-war/target/deployed-it/**/results.md"));
		assertTrue(workflow.contains("skyve-war/target/deployed-it/**/browser/**"));
		assertFalse(workflow.contains("ThemeResolutionLifecycleIT"));
		assertTrue(caller.contains("uses: ./.github/workflows/deployed-it.yml"));
		assertTrue(caller.contains("test-class: org.skyve.impl.web.faces.ThemeResolutionLifecycleIT"));
		assertFalse(caller.contains("run:"));

		Matcher actions = ACTION.matcher(workflow);
		List<String> versions = new ArrayList<>();
		while (actions.find()) {
			versions.add(actions.group(1));
		}
		assertFalse(versions.isEmpty());
		for (String version : versions) {
			assertTrue(version.matches("[0-9a-f]{40}"), version);
		}
	}

	@Test
	void standardWorkflowsReportFailsafeXmlAlongsideSurefire() throws Exception {
		for (String workflow : List.of("maven.yml", "sonar-analysis.yml")) {
			String source = Files.readString(REPOSITORY.resolve(".github/workflows").resolve(workflow));
			assertTrue(source.contains("**/target/surefire-reports/TEST-*.xml"));
			assertTrue(source.contains("**/target/failsafe-reports/TEST-*.xml"));
		}
	}

	private static Document xml(Path path) throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		return factory.newDocumentBuilder().parse(path.toFile());
	}

	private static Element profile(Document document, String id) {
		NodeList profiles = document.getElementsByTagName("profile");
		for (int i = 0, size = profiles.getLength(); i < size; i++) {
			Element profile = (Element) profiles.item(i);
			if (id.equals(descendant(profile, "id"))) {
				return profile;
			}
		}
		throw new AssertionError("Profile not found: " + id);
	}

	private static Element firstDirectChild(Element parent, String name) {
		NodeList children = parent.getChildNodes();
		for (int i = 0, size = children.getLength(); i < size; i++) {
			Node child = children.item(i);
			if ((child instanceof Element element) && name.equals(element.getTagName())) {
				return element;
			}
		}
		return null;
	}

	private static String descendant(Element element, String name) {
		NodeList values = element.getElementsByTagName(name);
		return (values.getLength() == 0) ? "" : values.item(0).getTextContent().trim();
	}
}
