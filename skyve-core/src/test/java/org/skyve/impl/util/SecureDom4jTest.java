package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import javax.xml.parsers.ParserConfigurationException;

import org.dom4j.io.SAXReader;
import org.junit.jupiter.api.Test;
import org.xml.sax.SAXException;

import javax.xml.validation.SchemaFactory;

@SuppressWarnings("static-method")
class SecureDom4jTest {

	@Test
	void newSAXReaderReturnsNonNull() throws SAXException, ParserConfigurationException {
		SAXReader reader = SecureDom4j.newSAXReader();
		assertNotNull(reader);
	}

	@Test
	void newSchemaFactoryReturnsNonNull() throws SAXException {
		SchemaFactory factory = SecureDom4j.newSchemaFactory();
		assertNotNull(factory);
	}
}
