package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.StringReader;
import java.nio.file.Path;

import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.junit.jupiter.api.Test;
import org.xml.sax.SAXException;

class ViewWidgetSchemaTest {
	private static final String VIEW_NAMESPACE = "http://www.skyve.org/xml/view";

	@Test
	@SuppressWarnings("static-method")
	void contentWidgetIsAccepted() throws Exception {
		assertDoesNotThrow(() -> validate("<content xmlns=\"" + VIEW_NAMESPACE + "\" binding=\"media\" />"));
	}

	@Test
	@SuppressWarnings("static-method")
	void contentImageWidgetIsRejected() throws Exception {
		assertThrows(SAXException.class, () -> validate("<contentImage xmlns=\"" + VIEW_NAMESPACE + "\" binding=\"image\" />"));
	}

	@Test
	@SuppressWarnings("static-method")
	void contentLinkWidgetIsRejected() throws Exception {
		assertThrows(SAXException.class, () -> validate("<contentLink xmlns=\"" + VIEW_NAMESPACE + "\" binding=\"media\" />"));
	}

	private static void validate(String xml) throws Exception {
		Path schemaPath = Path.of("src/test/resources/schemas/view.xsd");
		SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		Schema schema = factory.newSchema(schemaPath.toFile());
		Validator validator = schema.newValidator();
		validator.validate(new StreamSource(new StringReader(xml)));
	}
}
