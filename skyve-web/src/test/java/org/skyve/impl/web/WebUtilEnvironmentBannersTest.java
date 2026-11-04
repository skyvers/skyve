package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

/**
 * Tests the environment banner markup shared by the JSPs, Faces templates and SmartClient desktop.
 */
@SuppressWarnings("static-method")
class WebUtilEnvironmentBannersTest {
	private String originalIdentifier;

	@BeforeEach
	void rememberIdentifier() {
		originalIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
	}

	@AfterEach
	void restoreIdentifier() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = originalIdentifier;
	}

	@Test
	void noIdentifierRendersNothing() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = null;
		assertNull(WebUtil.getEnvironmentBannersHtml());

		UtilImpl.ENVIRONMENT_IDENTIFIER = "  ";
		assertNull(WebUtil.getEnvironmentBannersHtml());
	}

	@Test
	void identifierRendersBothBanners() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "UAT";

		String html = WebUtil.getEnvironmentBannersHtml();

		assertEquals("<div class=\"skyveEnvBanner skyveTopEnvBanner\">UAT</div>" +
						"<div class=\"skyveEnvBanner skyveBottomEnvBanner\">UAT</div>",
						html);
	}

	@Test
	void identifierIsHtmlEscaped() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "<b>UAT</b>";

		assertEquals("<div class=\"skyveEnvBanner skyveTopEnvBanner\">&lt;b&gt;UAT&lt;/b&gt;</div>" +
						"<div class=\"skyveEnvBanner skyveBottomEnvBanner\">&lt;b&gt;UAT&lt;/b&gt;</div>",
						WebUtil.getEnvironmentBannersHtml());
	}
}
