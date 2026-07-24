package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.IOException;
import java.nio.file.Path;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

@SuppressWarnings("static-method")
class ThemeWebXmlStructureTest {
	@Test
	void obsoleteThemeFilterIsNotRegistered() throws Exception {
		Document document = webXml();
		assertFalse(containsFilterReference(document, "FacesThemeSelectionFilter"));
	}

	@Test
	void primeFacesThemeUsesOnlyCoordinatorBackedFacade() throws Exception {
		Document document = webXml();
		NodeList parameters = document.getElementsByTagName("context-param");
		for (int i = 0, length = parameters.getLength(); i < length; i++) {
			Element parameter = (Element) parameters.item(i);
			if ("primefaces.THEME".equals(text(parameter, "param-name"))) {
				assertEquals("#{_skyveTheme.componentThemeName}", text(parameter, "param-value"));
				return;
			}
		}
		throw new AssertionError("primefaces.THEME context parameter not found");
	}

	private static Document webXml() throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		return factory.newDocumentBuilder().parse(Path.of("src/main/webapp/WEB-INF/web.xml").toFile());
	}

	private static boolean containsFilterReference(Document document, String filterName) {
		NodeList names = document.getElementsByTagName("filter-name");
		for (int i = 0, length = names.getLength(); i < length; i++) {
			if (filterName.equals(names.item(i).getTextContent().trim())) {
				return true;
			}
		}
		return false;
	}

	private static String text(Element element, String tagName) {
		NodeList nodes = element.getElementsByTagName(tagName);
		return (nodes.getLength() == 0) ? null : nodes.item(0).getTextContent().trim();
	}
}
