package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

@SuppressWarnings({ "static-method", "java:S1192" }) // Repeated literals are deliberate resource structure contracts.
class DeviceResourcesTest {
	private static final Path RESOURCES = Path.of("src/main/resources/META-INF/resources");

	@Test
	void deviceJspConsumesEmulationCommandAndRedirectsToCleanContextUrl() throws Exception {
		String source = Files.readString(RESOURCES.resolve("device.jsp"));

		assertTrue(source.contains("WebUtil.appendRequestParameters(targetUrl, request, " +
										"AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER)"));
		assertTrue(source.contains("UserAgent.consumeDevicePreviewCommand(request)"));
		assertTrue(source.contains("HttpServletResponse.SC_SEE_OTHER"));
		assertTrue(source.contains("response.setHeader(\"Location\""));
		assertTrue(source.contains("new StringBuilder(request.getContextPath()).append('/')"));
		assertFalse(source.contains("append(\"/home.jsp\")"));
		assertFalse(source.contains("request.getParameterNames()"));
		assertFalse(source.contains("FacesThemeEmulation"));
		assertFalse(source.contains("request.getParameter(name)"));
	}

	@Test
	void devicePageUsesVersionedMinifiedResourcesAndPassThroughConfiguration() throws Exception {
		String source = Files.readString(RESOURCES.resolve("device.xhtml"));

		assertTrue(source.contains("xmlns:pt=\"http://xmlns.jcp.org/jsf/passthrough\""));
		assertTrue(source.contains("pt:data-preview-url-prefix=\"#{device.previewUrlPrefix}\""));
		assertTrue(source.contains("pt:data-end-preview-url=\"#{device.endPreviewUrl}\""));
		assertTrue(source.contains("skyve/css/device-min.css?v=#{device.webResourceFileVersion}"));
		assertTrue(source.contains("skyve/prime/device-min.js?v=#{device.webResourceFileVersion}"));
		assertTrue(source.contains("<p:speedDial"));
		assertTrue(source.contains("<p:growl"));
		assertFalse(source.contains("<style"));
		assertFalse(source.contains(" style="));
		assertFalse(source.contains(" onclick="));
		assertFalse(source.contains("sessionStorage"));
		assertFalse(source.contains("primevue"));
		assertFalse(source.contains("createApp"));
		assertFalse(source.contains("clickDeviceJspUrl"));
	}

	@Test
	void deviceScriptBindsToSpeedDialMarkupRenderedByPrimeFaces() throws Exception {
		String page = Files.readString(RESOURCES.resolve("device.xhtml"));
		String script = Files.readString(Path.of("src/js/prime/device.js"));
		String packagedScript = Files.readString(RESOURCES.resolve("skyve/prime/device-min.js"));

		for (String action : new String[] { "Phone", "Tablet", "Laptop", "Desktop", "End" }) {
			assertTrue(page.contains("id=\"devicePreview" + action + "\""));
			assertTrue(script.contains(".ui-speeddial-action[id$=\"devicePreview" + action + "\"]"));
			assertTrue(packagedScript.contains("devicePreview" + action));
		}
		assertFalse(page.contains("styleClass=\"device-preview-"));
	}

	@Test
	void deviceReadableAndGeneratedAssetsExist() {
		assertTrue(Files.exists(Path.of("src/css/device.css")));
		assertTrue(Files.exists(Path.of("src/js/prime/device.js")));
		assertTrue(Files.exists(RESOURCES.resolve("skyve/css/device-min.css")));
		assertTrue(Files.exists(RESOURCES.resolve("skyve/prime/device-min.js")));
		assertTrue(Files.exists(RESOURCES.resolve("skyve/prime/device-min.js.map")));
	}

	@Test
	void deviceShimsUseSpaceGreyAndHighContrastDarkModeFinishes() throws Exception {
		String css = Files.readString(Path.of("src/css/device.css"));
		String packagedCss = Files.readString(RESOURCES.resolve("skyve/css/device-min.css"));

		assertTrue(css.contains("--device-shim-color: #4b5058"));
		assertTrue(css.contains("@media (prefers-color-scheme: dark)"));
		assertTrue(css.contains("--device-shim-color: #b8bec8"));
		assertTrue(css.contains("border-color: var(--device-shim-color)"));
		assertTrue(packagedCss.contains("--device-shim-color:#4b5058"));
		assertTrue(packagedCss.contains("prefers-color-scheme:dark"));
		assertTrue(packagedCss.contains("--device-shim-color:#b8bec8"));
	}

	@Test
	void phoneAndTabletHomeButtonsHaveARecessedHardwareFinish() throws Exception {
		String css = Files.readString(Path.of("src/css/device.css"));
		String packagedCss = Files.readString(RESOURCES.resolve("skyve/css/device-min.css"));

		assertTrue(css.contains("--device-button-ring-color"));
		assertTrue(css.contains("background: radial-gradient(circle at 35% 30%"));
		assertTrue(css.contains("box-shadow: inset 0 1px 2px"));
		assertTrue(css.contains("width: 44px"));
		assertTrue(css.contains("bottom: -74px"));
		assertTrue(packagedCss.contains("radial-gradient(circle at 35% 30%"));
		assertTrue(packagedCss.contains("--device-button-ring-color"));
		assertTrue(packagedCss.contains("width:44px;height:44px"));
		assertTrue(packagedCss.contains("bottom:-74px"));
	}

	@Test
	void laptopUsesARoundedDisplayAndTaperedMetalBase() throws Exception {
		String css = Files.readString(Path.of("src/css/device.css"));
		String script = Files.readString(Path.of("src/js/prime/device.js"));
		String packagedCss = Files.readString(RESOURCES.resolve("skyve/css/device-min.css"));
		String packagedScript = Files.readString(RESOURCES.resolve("skyve/prime/device-min.js"));

		assertTrue(css.contains("border-radius: 14px 14px 8px 8px"));
		assertTrue(css.contains("transform: translateX(-50%) perspective(825px) rotateX(55deg)"));
		assertTrue(css.contains("transform-origin: top center"));
		assertTrue(css.contains("border-radius: 0 0 14px 14px"));
		assertTrue(css.contains("--device-laptop-base-highlight-color"));
		assertTrue(script.contains("tapered base width (1540px)"));
		assertTrue(script.contains("horizontalScale = (parent.width() - 40) / 1540"));
		assertTrue(packagedCss.contains("--device-laptop-base-highlight-color"));
		assertTrue(packagedCss.contains("translate(-50%)perspective(825px)rotateX(55deg)"));
		assertTrue(packagedScript.contains("/1540"));
	}
}
