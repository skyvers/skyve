package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

@SuppressWarnings({ "static-method", "java:S1192" }) // Repeated literals are deliberate routing structure contracts.
class ThemeRoutingStructureTest {
	@Test
	void deployedOverlayOwnsItsDirectRouteWithoutChangingApplicationRouting() throws Exception {
		String router = Files.readString(Path.of("src/main/java/router/router.xml"));
		String installer = Files.readString(Path.of(
				"src/deployed-it/java/util/deployed/transport/theme/ThemeResolutionRouteInstaller.java"));

		assertFalse(router.contains("/deployed-it/theme-lifecycle.xhtml"), router);
		assertFalse(router.contains("<direct path=\"/external/startup.xhtml\""), router);
		assertFalse(router.contains("<direct path=\"/external/\""), router);
		assertTrue(installer.contains("new FluentRouter(router).addDirect(new FluentDirect()"), installer);
		assertTrue(installer.contains(".path(\"/deployed-it/theme-lifecycle.xhtml\")"), installer);
		assertTrue(installer.contains(".uxui(\"phone\")"), installer);
	}

	@Test
	void builtInInternalNavigationDoesNotPropagateEmulation() throws Exception {
		Path templates = Path.of("src/main/webapp/WEB-INF/pages/templates");
		String editorial = Files.readString(templates.resolve("editorial/template.xhtml"));
		String external = Files.readString(templates.resolve("external/template.xhtml"));
		String ecuadorTemplate = Files.readString(templates.resolve("ecuador/template.xhtml"));
		String ecuadorTopbar = Files.readString(templates.resolve("ecuador/topbar.xhtml"));
		String ultimaTopbar = Files.readString(templates.resolve("ultima/topbar.xhtml"));
		String diamond = Files.readString(templates.resolve("diamond/template.xhtml"));

		assertProfileAndAccountAreRaw(editorial);
		assertProfileAndAccountAreRaw(external);
		assertProfileAndAccountAreRaw(ecuadorTopbar);
		assertProfileAndAccountAreRaw(ultimaTopbar);
		assertFalse(ecuadorTemplate.contains("getEmulationAwareUrl"));
		assertFalse(ultimaTopbar.contains("getEmulationAwareUrl"));
		assertFalse(diamond.contains("getEmulationAwareUrl"));
	}

	@Test
	void homeDoesNotConsumeTheDevicePreviewCommand() throws Exception {
		String home = Files.readString(Path.of("src/main/webapp/home.jsp"));
		assertFalse(home.contains("consumeDevicePreviewCommand"));
		assertTrue(home.contains("WebUtil.appendRequestParameters(loginUrl, request, " +
									"AbstractWebContext.CUSTOMER_COOKIE_NAME)"));
		assertFalse(home.contains("request.getParameterNames()"));
	}

	private static void assertProfileAndAccountAreRaw(String source) {
		assertTrue(source.contains("url=\"?a=e&amp;m=admin&amp;d=UserDashboard\""));
		assertTrue(source.contains("url=\"?a=e&amp;m=admin&amp;d=UserAccount\""));
		assertFalse(source.contains("getEmulationAwareUrl"));
	}
}
