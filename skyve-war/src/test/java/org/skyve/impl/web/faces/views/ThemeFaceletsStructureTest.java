package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.router.UxUi;

@SuppressWarnings({ "static-method", "java:S1192" }) // Repeated literals are deliberate Facelets structure contracts.
class ThemeFaceletsStructureTest {
	private static final String IFRAME_TEMPLATE_PATH = "WEB-INF/pages/templates/iframe/template.xhtml";
	private static final List<String> FAMILIES = List.of("editorial", "external", "ecuador", "ultima", "diamond");
	private static final List<String> DYNAMIC_CLIENT_PAGES = List.of("external/home.xhtml",
			"external/edit.xhtml",
			"external/list.xhtml",
			"external/map.xhtml",
			"external/public.xhtml",
			"external/selfRegister.xhtml",
			"external/startup.xhtml",
			"unsubscribe.xhtml");

	@Test
	void popupPagesUseSharedIframeTemplateAndRetainCallbacksAndBodyScripts() throws IOException {
		String upload = read("upload.xhtml");
		String bizImport = read("bizImport.xhtml");
		String imageMarkup = read("imageMarkup.xhtml");

		assertSharedIframePage(upload);
		assertTrue(upload.contains("var owner = SKYVE.Util.findSmartClientWindow();"), upload);
		assertTrue(upload.contains("oncomplete=\"skyveAfterActionUpload('#{_skyveContent.uploadState.uploadKind}')\""), upload);
		assertFalse(upload.contains("window.parent.isc"), upload);

		assertSharedIframePage(bizImport);
		assertTrue(bizImport.contains("var owner = SKYVE.Util.findSmartClientWindow();"), bizImport);
		assertTrue(bizImport.contains("oncomplete=\"skyveAfterBizImport()\""), bizImport);
		assertFalse(bizImport.contains("window.parent.isc"), bizImport);

		assertSharedIframePage(imageMarkup);
		assertTrue(imageMarkup.contains("<ui:define name=\"templateBodyScripts\">"), imageMarkup);
		assertTrue(imageMarkup.contains("skyve/css/image-markup-min.css?v=#{_skyveMarkup.webResourceFileVersion}"),
				imageMarkup);
		assertTrue(imageMarkup.contains("skyve/prime/image-markup-min.js?v=#{_skyveMarkup.webResourceFileVersion}"),
				imageMarkup);
		assertTrue(imageMarkup.contains("pt:data-image-width=\"#{_skyveMarkup.imageWidth}\""), imageMarkup);
		assertTrue(imageMarkup.contains("pt:data-image-height=\"#{_skyveMarkup.imageHeight}\""), imageMarkup);
		assertTrue(imageMarkup.contains("pt:data-background-url=\"#{_skyveMarkup.backgroundUrl}\""), imageMarkup);
		assertTrue(imageMarkup.contains("pt:data-forced-dark=\"#{_skyveTheme.forcedDark}\""), imageMarkup);
		assertTrue(imageMarkup.contains("pt:data-smart-client=\"#{_skyveTheme.smartClient}\""), imageMarkup);
		assertFalse(imageMarkup.contains("<style>"), imageMarkup);
		assertFalse(imageMarkup.contains("function applyMarkup()"), imageMarkup);
		assertFalse(imageMarkup.contains("theme: 'light'"), imageMarkup);
		assertFalse(imageMarkup.contains("window.parent.isc"), imageMarkup);
		assertFalse(imageMarkup.contains("top.SKYVE"), imageMarkup);

		String css = Files.readString(repositoryRoot().resolve("skyve-web/src/css/image-markup.css"));
		String script = Files.readString(repositoryRoot().resolve("skyve-web/src/js/prime/image-markup.js"));
		String packagedCss = Files.readString(repositoryRoot().resolve(
				"skyve-web/src/main/resources/META-INF/resources/skyve/css/image-markup-min.css"));
		String packagedScript = Files.readString(repositoryRoot().resolve(
				"skyve-web/src/main/resources/META-INF/resources/skyve/prime/image-markup-min.js"));
		assertTrue(css.contains("var(--skyve-editorial-button-bg, var(--primary-color, #007ad9))"), css);
		assertTrue(css.contains("var(--skyve-editorial-button-fg, var(--primary-color-text, #fff))"), css);
		assertTrue(css.contains("var(--skyve-editorial-button-hover-bg, var(--primary-color, #116fbf))"), css);
		assertTrue(script.contains("(! smartClient) && (forcedDark || darkModePreference.matches)"), script);
		assertTrue(script.contains("theme: excalidrawTheme()"), script);
		assertTrue(packagedCss.contains("--skyve-editorial-button-bg"), packagedCss);
		assertTrue(packagedScript.contains("image-markup-min.js.map"), packagedScript);
	}

	@Test
	void iframeUsesAtomicResolvedResourceFamilyBeforePageOverrides() throws IOException {
		String template = read(IFRAME_TEMPLATE_PATH);
		String headInclude = "concat(_skyveTheme.iframeResourceTemplateName).concat('/head-resources.xhtml')";
		String bodyInclude = "concat(_skyveTheme.iframeResourceTemplateName).concat('/body-resources.xhtml')";

		assertTrue(template.contains(headInclude), template);
		assertTrue(template.contains(bodyInclude), template);
		assertFalse(template.contains("concat(_skyveTheme.templateName).concat('/head-resources.xhtml')"), template);
		assertFalse(template.contains("concat(_skyveTheme.templateName).concat('/body-resources.xhtml')"), template);
		assertTrue(template.indexOf(headInclude) < template.indexOf("<ui:insert name=\"templateHead\" />"), template);
		assertTrue(template.indexOf(bodyInclude) < template.indexOf("<ui:insert name=\"templateHead\" />"), template);
		assertTrue(template.contains("<ui:param name=\"bean\" value=\"#{bean}\" />"), template);
		assertTrue(template.contains("<ui:param name=\"iframe\" value=\"true\" />"), template);
		assertTrue(template.contains("#{_skyveTheme.layoutStyleClass}"), template);
		assertFalse(template.contains("#{_skyveTheme.themeColour}"), template);
		assertTrue(template.contains("<ui:insert name=\"templateBodyScripts\" />"), template);
		assertTrue(template.contains("height: 100% !important;"), template);
	}

	@Test
	void genericIframeResourcePairIsComplete() throws IOException {
		String head = read("WEB-INF/pages/templates/iframe/head-resources.xhtml");
		String body = read("WEB-INF/pages/templates/iframe/body-resources.xhtml");
		String template = read(IFRAME_TEMPLATE_PATH);

		assertTrue(head.contains("skyve/css/prime-min.css"), head);
		assertTrue(head.contains("editorial/assets/css/main-min.css"), head);
		assertTrue(head.contains("pages/css/admin.css"), head);
		assertTrue(head.contains("skyve/prime/skyve-min.js"), head);
		assertFalse(head.contains(IFRAME_TEMPLATE_PATH), head);
		assertFalse(body.contains(IFRAME_TEMPLATE_PATH), body);
		assertFalse(template.contains("client-emulation.xhtml"), template);
	}

	@Test
	void fullTemplatesRetainFamilyResourceOrder() throws IOException {
		for (String family : FAMILIES) {
			String template = read("WEB-INF/pages/templates/" + family + "/template.xhtml");
			String headResources = read("WEB-INF/pages/templates/" + family + "/head-resources.xhtml");
			String bodyResources = read("WEB-INF/pages/templates/" + family + "/body-resources.xhtml");

			int head = template.indexOf("<ui:include src=\"./head-resources.xhtml\">");
			int pageHead = template.indexOf("<ui:insert name=\"templateHead\" />");
			int body = template.indexOf("<ui:include src=\"./body-resources.xhtml\">");
			assertTrue((head >= 0) && (head < pageHead), family + ": " + template);
			assertTrue((body > pageHead) && (body < template.indexOf("</h:body>")), family + ": " + template);
			assertFalse(template.contains("client-emulation.xhtml"), family + ": " + template);
			assertTrue(template.contains("<ui:param name=\"iframe\" value=\"false\" />"), family + ": " + template);
			assertTrue(template.contains("styleClass=\"skyve-global-messages\""), family + ": " + template);
			assertTrue(headResources.contains("skyve/prime/skyve-min.js?v=#{bean.webResourceFileVersion}"),
					family + ": " + headResources);
			assertFalse(headResources.contains("<c:if"), family + ": " + headResources);
			assertFalse(bodyResources.contains("<c:if"), family + ": " + bodyResources);
		}
	}

	@Test
	void resolvedPropertiesReplaceFamilyParsingAndFallbacks() throws IOException {
		String editorialHead = read("WEB-INF/pages/templates/editorial/head-resources.xhtml");
		String externalHead = read("WEB-INF/pages/templates/external/head-resources.xhtml");
		String ecuadorBody = read("WEB-INF/pages/templates/ecuador/body-resources.xhtml");
		String ultima = read("WEB-INF/pages/templates/ultima/template.xhtml");
		String diamond = read("WEB-INF/pages/templates/diamond/template.xhtml");
		String diamondBody = read("WEB-INF/pages/templates/diamond/body-resources.xhtml");

		assertTrue(editorialHead.contains("_skyveTheme.baseColour"), editorialHead);
		assertTrue(editorialHead.contains("_skyveTheme.forcedDark"), editorialHead);
		assertTrue(editorialHead.contains("not _skyveTheme.smartClient"), editorialHead);
		assertTrue(externalHead.contains("_skyveTheme.baseColour"), externalHead);
		assertTrue(externalHead.contains("not _skyveTheme.smartClient"), externalHead);
		assertTrue(ecuadorBody.contains("_skyveTheme.baseColour"), ecuadorBody);
		assertTrue(ultima.contains("_skyveTheme.baseColour"), ultima);
		assertTrue(ultima.contains("_skyveTheme.layoutStyleClass"), ultima);
		assertTrue(diamond.contains("_skyveTheme.layoutStyleClass"), diamond);
		assertTrue(diamondBody.contains("_skyveTheme.scheme"), diamondBody);
	}

	@Test
	void dynamicClientPagesUseOnlyResolvedTemplateName() throws IOException {
		String expected = "concat(_skyveTheme.templateName).concat('/view.xhtml')";
		for (String pageName : DYNAMIC_CLIENT_PAGES) {
			String page = read(pageName);
			assertTrue(page.contains(expected), pageName + ": " + page);
			assertEquals(1, occurrences(page, expected), pageName + ": " + page);
			assertFalse(page.contains("skyve.userAgentType"), pageName + ": " + page);
		}
	}

	@Test
	void allWarFaceletsRejectRawThemeLogicAndEmulationPropagation() throws IOException {
		try (Stream<Path> files = Files.walk(webappRoot())) {
			for (Path file : files.filter(path -> path.toString().endsWith(".xhtml")).toList()) {
				String page = Files.readString(file);
				String description = file + ": " + page;
				assertFalse(page.matches("(?s).*(skyve|skyvePublic|skyveStartup|adminUnsubscribe)\\.templateName.*"), description);
				assertFalse(page.contains("getThemeColour("), description);
				assertFalse(page.contains("pfTheme"), description);
				assertFalse(page.contains(".split('-')"), description);
				assertFalse(page.contains("_ua"), description);
				assertFalse(page.contains("device.jsp?ua="), description);
				for (String line : page.lines().toList()) {
					if ((line.contains("value=\"Profile\"") || line.contains("value=\"Account\"")) && line.contains("url=")) {
						assertFalse(line.contains("getEmulationAwareUrl("), file + ": " + line);
					}
				}
			}
		}
	}

	@Test
	void developerGuideExamplesTrackPublicApisAndDeployedCommands() throws IOException {
		Path guidePath = repositoryRoot().resolve("docs/theme-resolution.md");
		String guide = Files.readString(guidePath);

		UxUi desktop = UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Tahoe", "casablanca", "smartclient");
		UxUi vendor = UxUi.newPrimeFaces("tablet", "vendor-template", "vendor-theme", "indigo");

		assertNull(desktop.getPfTemplateName());
		assertEquals("indigo", vendor.getPfThemeColour());
		assertTrue(guide.contains("<direct path=\"/special/welcome.xhtml\" uxui=\"specialWelcome\" />"), guide);
		assertTrue(guide.contains("public UxUi resolve(String name)"), guide);
		assertTrue(guide.contains("UxUi.newPrimeFaces(\"phone\", \"editorial\", \"skyve\", \"blue\")"), guide);
		assertTrue(guide.contains("UxUi.newSmartClient("), guide);
		assertTrue(guide.contains("SKYVE_DEPLOYED_IT_TEST=example.ReusableHarnessIT tools/deployed-it/run.sh --validate-only"),
				guide);
		assertTrue(guide.contains(
				"SKYVE_DEPLOYED_IT_TEST=org.skyve.impl.web.faces.ThemeResolutionLifecycleIT"), guide);
		assertTrue(Files.readString(repositoryRoot().resolve("skyve-war/pom.xml"))
				.contains("<id>deployed-it</id>"));
		assertTrue(Files.isRegularFile(repositoryRoot().resolve("tools/deployed-it/run.sh")));
	}

	private static void assertSharedIframePage(String page) {
		assertTrue(page.contains("template=\"/WEB-INF/pages/templates/iframe/template.xhtml\""), page);
		assertFalse(page.contains("skyve/prime/skyve-min.js"), page);
	}

	private static int occurrences(String value, String token) {
		int result = 0;
		int offset = 0;
		while ((offset = value.indexOf(token, offset)) >= 0) {
			result++;
			offset += token.length();
		}
		return result;
	}

	private static String read(String relativePath) throws IOException {
		return Files.readString(webappRoot().resolve(relativePath));
	}

	private static Path webappRoot() {
		Path fromModule = Path.of("src/main/webapp");
		return Files.isDirectory(fromModule) ? fromModule : Path.of("skyve-war/src/main/webapp");
	}

	private static Path repositoryRoot() {
		Path fromModule = Path.of("..").toAbsolutePath().normalize();
		return Files.isRegularFile(fromModule.resolve("docs/theme-resolution.md"))
				? fromModule
				: Path.of("").toAbsolutePath().normalize();
	}
}
