package org.skyve.impl.web.filter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.nio.file.Path;

import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockFilterConfig;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The security headers a response really carries when the filter is configured as {@code web.xml}
 * configures it: framing is limited to the same origin, plug-ins and foreign form targets are refused,
 * and the browser is told not to sniff content types. The CSP upgrades requests only over TLS.
 */
@SuppressWarnings("static-method")
class SecurityHeadersContentTest {
	private String serverUrl;

	@BeforeEach
	void rememberServerUrl() throws Exception {
		serverUrl = UtilImpl.SERVER_URL;
		resetSecureUrlCache();
	}

	@AfterEach
	void restoreServerUrl() throws Exception {
		UtilImpl.SERVER_URL = serverUrl;
		resetSecureUrlCache();
	}

	@Test
	void theResponseCarriesTheFramingAndContentTypeHeaders() throws Exception {
		UtilImpl.SERVER_URL = "https://app.example.com";

		MockHttpServletResponse response = filter();

		assertEquals("SAMEORIGIN", response.getHeader("X-Frame-Options"));
		assertEquals("nosniff", response.getHeader("X-Content-Type-Options"));
		assertEquals("same-origin", response.getHeader("Referrer-Policy"));
		assertEquals("none", response.getHeader("X-Permitted-Cross-Domain-Policies"));
	}

	@Test
	void theContentSecurityPolicyLimitsFramingPluginsBaseAndFormTargets() throws Exception {
		UtilImpl.SERVER_URL = "https://app.example.com";

		String csp = filter().getHeader("Content-Security-Policy");

		assertNotNull(csp);
		for (String directive : new String[] { "default-src 'self'", "frame-ancestors 'self'", "object-src 'none'",
				"base-uri 'self'",
				"form-action 'self'" }) {
			assertTrue(csp.contains(directive), directive + " in " + csp);
		}
	}

	@Test
	void theContentSecurityPolicyUpgradesInsecureRequestsOnlyOverTls() throws Exception {
		UtilImpl.SERVER_URL = "https://app.example.com";
		assertTrue(filter().getHeader("Content-Security-Policy").contains("upgrade-insecure-requests"));

		UtilImpl.SERVER_URL = "http://localhost:8080";
		resetSecureUrlCache();
		assertFalse(filter().getHeader("Content-Security-Policy").contains("upgrade-insecure-requests"));
	}

	/**
	 * {@code Util.isSecureUrl()} remembers its first answer.
	 */
	private static void resetSecureUrlCache() throws Exception {
		Field field = Util.class.getDeclaredField("secureUrl");
		field.setAccessible(true);
		field.set(null, null);
	}

	/**
	 * Runs {@link ResponseHeaderFilter} configured with the init parameters of {@code web.xml}'s SecurityHeadersFilter.
	 */
	private static MockHttpServletResponse filter() throws Exception {
		MockFilterConfig config = new MockFilterConfig();
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		Document document = factory.newDocumentBuilder().parse(Path.of("src/main/webapp/WEB-INF/web.xml").toFile());
		NodeList filters = document.getElementsByTagName("filter");
		for (int i = 0, length = filters.getLength(); i < length; i++) {
			Element filter = (Element) filters.item(i);
			if (ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME
					.equals(filter.getElementsByTagName("filter-name").item(0).getTextContent().trim())) {
				NodeList params = filter.getElementsByTagName("init-param");
				for (int j = 0, paramLength = params.getLength(); j < paramLength; j++) {
					Element param = (Element) params.item(j);
					config.addInitParameter(param.getElementsByTagName("param-name").item(0).getTextContent().trim(),
							param.getElementsByTagName("param-value").item(0).getTextContent().trim());
				}
			}
		}
		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		filter.init(config);
		MockHttpServletResponse result = new MockHttpServletResponse();
		filter.doFilter(new MockHttpServletRequest(), result, new MockFilterChain());
		return result;
	}
}
