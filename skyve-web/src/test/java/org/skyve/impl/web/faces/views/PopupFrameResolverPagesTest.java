package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class PopupFrameResolverPagesTest {
	private static final Path UPLOAD_PAGE = webappPage("upload.xhtml");
	private static final Path BIZ_IMPORT_PAGE = webappPage("bizImport.xhtml");
	private static final Path IMAGE_MARKUP_PAGE = webappPage("imageMarkup.xhtml");
	private static final Path IFRAME_TEMPLATE = webappPage("WEB-INF/pages/templates/iframe/template.xhtml");
	private static final Path EDITORIAL_HEAD_RESOURCES = webappPage("WEB-INF/pages/templates/editorial/head-resources.xhtml");
	private static final Path EDITORIAL_BODY_RESOURCES = webappPage("WEB-INF/pages/templates/editorial/body-resources.xhtml");

	@Test
	void uploadPageUsesSharedIframeTemplateAndRetainsActionCallback() throws IOException {
		String page = Files.readString(UPLOAD_PAGE);

		assertTrue(page.contains("template=\"/WEB-INF/pages/templates/iframe/template.xhtml\""), page);
		assertFalse(page.contains("skyve/prime/skyve-min.js"), page);
		assertTrue(page.contains("var owner = SKYVE.Util.findSmartClientWindow();"), page);
		assertTrue(page.contains("oncomplete=\"skyveAfterActionUpload('#{_skyveContent.uploadState.uploadKind}')\""), page);
		assertFalse(page.contains("window.parent.isc"), page);
	}

	@Test
	void bizImportPageUsesSharedIframeTemplateAndRetainsCompletionCallback() throws IOException {
		String page = Files.readString(BIZ_IMPORT_PAGE);

		assertTrue(page.contains("template=\"/WEB-INF/pages/templates/iframe/template.xhtml\""), page);
		assertFalse(page.contains("skyve/prime/skyve-min.js"), page);
		assertTrue(page.contains("var owner = SKYVE.Util.findSmartClientWindow();"), page);
		assertTrue(page.contains("oncomplete=\"skyveAfterBizImport()\""), page);
		assertFalse(page.contains("window.parent.isc"), page);
	}

	@Test
	void imageMarkupPageUsesSharedIframeTemplateAndRetainsBodyScripts() throws IOException {
		String page = Files.readString(IMAGE_MARKUP_PAGE);

		assertTrue(page.contains("template=\"/WEB-INF/pages/templates/iframe/template.xhtml\""), page);
		assertTrue(page.contains("<ui:define name=\"templateBodyScripts\">"), page);
		assertFalse(page.contains("skyve/prime/skyve-min.js"), page);
		assertFalse(page.contains("window.parent.isc"), page);
		assertFalse(page.contains("top.SKYVE"), page);
	}

	@Test
	void iframeTemplateLoadsResolvedCompleteThemeBeforePageOverrides() throws IOException {
		String template = Files.readString(IFRAME_TEMPLATE);
		String editorialHeadResources = Files.readString(EDITORIAL_HEAD_RESOURCES);
		String editorialBodyResources = Files.readString(EDITORIAL_BODY_RESOURCES);

		String headResourceInclude = "concat(_skyveTheme.templateName).concat('/head-resources.xhtml')";
		String bodyResourceInclude = "concat(_skyveTheme.templateName).concat('/body-resources.xhtml')";
		assertTrue(template.contains(headResourceInclude), template);
		assertTrue(template.contains(bodyResourceInclude), template);
		assertTrue(template.indexOf(headResourceInclude) < template.indexOf("<ui:insert name=\"templateHead\" />"), template);
		assertTrue(template.indexOf(bodyResourceInclude) < template.indexOf("<ui:insert name=\"templateHead\" />"), template);
		assertTrue(template.contains("<ui:param name=\"iframe\" value=\"true\" />"), template);
		assertTrue(template.contains("<ui:insert name=\"templateBodyScripts\" />"), template);
		assertTrue(template.contains("height: 100% !important;"), template);
		assertTrue(editorialHeadResources.contains("skyve/prime/skyve-min.js?v=#{bean.webResourceFileVersion}"), editorialHeadResources);
		assertTrue(editorialHeadResources.contains("pages/css/admin.css"), editorialHeadResources);
		assertTrue(editorialHeadResources.contains("editorial/assets/css/main-min.css"), editorialHeadResources);
		assertTrue(editorialBodyResources.contains("<ui:fragment rendered=\"#{not iframe}\">"), editorialBodyResources);
	}

	@Test
	void fullTemplatesAndIframesShareExplicitFamilyResourceDefinitions() throws IOException {
		String[] families = {"editorial", "external", "ecuador", "ultima", "diamond"};
		for (String family : families) {
			String template = Files.readString(webappPage("WEB-INF/pages/templates/" + family + "/template.xhtml"));
			String headResources = Files.readString(webappPage("WEB-INF/pages/templates/" + family + "/head-resources.xhtml"));
			String bodyResources = Files.readString(webappPage("WEB-INF/pages/templates/" + family + "/body-resources.xhtml"));

			assertTrue(template.contains("<ui:include src=\"./head-resources.xhtml\">"), family + ": " + template);
			assertTrue(template.contains("<ui:include src=\"./body-resources.xhtml\">"), family + ": " + template);
			assertTrue(template.contains("<ui:param name=\"iframe\" value=\"false\" />"), family + ": " + template);
			assertTrue(template.contains("styleClass=\"skyve-global-messages\""), family + ": " + template);
			assertFalse(headResources.contains("<c:if"), family + ": " + headResources);
			assertFalse(bodyResources.contains("<c:if"), family + ": " + bodyResources);
		}
	}

	private static Path webappPage(String fileName) {
		Path fromModule = Path.of("../skyve-war/src/main/webapp", fileName);
		if (Files.exists(fromModule)) {
			return fromModule;
		}
		return Path.of("skyve-war/src/main/webapp", fileName);
	}
}
