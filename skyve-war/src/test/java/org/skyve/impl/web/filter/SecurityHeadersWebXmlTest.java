package org.skyve.impl.web.filter;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Verifies deployment-descriptor security-header filter mapping.
 */
@SuppressWarnings("static-method")
class SecurityHeadersWebXmlTest {
	@Test
	void securityHeadersFilterAppliesToRequestAndErrorDispatches() throws Exception {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		Document document = factory.newDocumentBuilder()
				.parse(Path.of("src/main/webapp/WEB-INF/web.xml").toFile());
		NodeList mappings = document.getElementsByTagName("filter-mapping");
		Set<String> dispatchers = new HashSet<>();

		for (int i = 0, length = mappings.getLength(); i < length; i++) {
			Element mapping = (Element) mappings.item(i);
			if (ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME.equals(text(mapping, "filter-name"))
					&& "/*".equals(text(mapping, "url-pattern"))) {
				NodeList dispatcherNodes = mapping.getElementsByTagName("dispatcher");
				for (int j = 0, dispatcherLength = dispatcherNodes.getLength(); j < dispatcherLength; j++) {
					dispatchers.add(dispatcherNodes.item(j).getTextContent().trim());
				}
			}
		}

		assertTrue(dispatchers.contains("REQUEST"), dispatchers::toString);
		assertTrue(dispatchers.contains("ERROR"), dispatchers::toString);
	}

	private static String text(Element element, String tagName) {
		NodeList nodes = element.getElementsByTagName(tagName);
		if (nodes.getLength() == 0) {
			return null;
		}
		Node node = nodes.item(0);
		return node.getTextContent().trim();
	}
}
