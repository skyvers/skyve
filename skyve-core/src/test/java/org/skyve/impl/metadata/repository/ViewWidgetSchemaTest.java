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

	@Test
	@SuppressWarnings("static-method")
	void boilerplateEscapeAttributesAreAcceptedOnSupportedViewElements() throws Exception {
		assertDoesNotThrow(() -> validate("<view xmlns=\"" + VIEW_NAMESPACE
											+ "\" name=\"edit\" title=\"Edit\" escapeTitle=\"false\">"
											+ "<actions>"
											+ "<save name=\"save\" displayName=\"Save\" toolTip=\"Tip\" confirm=\"Confirm\""
											+ " escapeDisplayName=\"false\" escapeToolTip=\"true\" escapeConfirm=\"false\" />"
											+ "</actions>"
											+ "</view>"));
		assertDoesNotThrow(() -> validate("<vbox xmlns=\"" + VIEW_NAMESPACE
											+ "\" borderTitle=\"Section\" escapeBorderTitle=\"false\" />"));
		assertDoesNotThrow(() -> validate("<hbox xmlns=\"" + VIEW_NAMESPACE
											+ "\" borderTitle=\"Section\" escapeBorderTitle=\"false\" />"));
		assertDoesNotThrow(() -> validate("<tab xmlns=\"" + VIEW_NAMESPACE
											+ "\" title=\"Tab\" escapeTitle=\"false\" />"));
		assertDoesNotThrow(() -> validate("<form xmlns=\"" + VIEW_NAMESPACE
											+ "\" borderTitle=\"Form\" escapeBorderTitle=\"false\">"
											+ "<column />"
											+ "<row><item label=\"Name\" requiredMessage=\"Required\" help=\"Help\""
											+ " escapeLabel=\"false\" escapeRequiredMessage=\"true\" escapeHelp=\"false\">"
											+ "<default binding=\"name\" />"
											+ "</item></row>"
											+ "</form>"));
		assertDoesNotThrow(() -> validate("<dataGrid xmlns=\"" + VIEW_NAMESPACE
											+ "\" binding=\"rows\" title=\"Rows\" escapeTitle=\"false\">"
											+ "<boundColumn binding=\"name\" title=\"Name\" escapeTitle=\"false\" />"
											+ "</dataGrid>"));
		assertDoesNotThrow(() -> validate("<listRepeater xmlns=\"" + VIEW_NAMESPACE
											+ "\" model=\"rows\" title=\"Rows\" escapeTitle=\"false\" />"));
		assertDoesNotThrow(() -> validate("<listMembership xmlns=\"" + VIEW_NAMESPACE
											+ "\" binding=\"members\" membersHeading=\"Members\" candidatesHeading=\"Candidates\""
											+ " escapeMembersHeading=\"false\" escapeCandidatesHeading=\"true\" />"));
		assertDoesNotThrow(() -> validate("<link xmlns=\"" + VIEW_NAMESPACE
											+ "\" value=\"Open\" escapeValue=\"false\">"
											+ "<externalReference href=\"https://example.test\" />"
											+ "</link>"));
		assertDoesNotThrow(() -> validate("<zoomIn xmlns=\"" + VIEW_NAMESPACE
											+ "\" binding=\"customer\" displayName=\"Open\" toolTip=\"Open customer\""
											+ " escapeDisplayName=\"false\" escapeToolTip=\"true\" />"));
		assertDoesNotThrow(() -> validate("<dialogButton xmlns=\"" + VIEW_NAMESPACE
											+ "\" displayName=\"OK\" escapeDisplayName=\"false\" />"));
	}

	@Test
	@SuppressWarnings("static-method")
	void boilerplateEscapeAttributesAreRejectedOnUnsupportedViewElements() {
		assertThrows(SAXException.class,
						() -> validate("<label xmlns=\"" + VIEW_NAMESPACE + "\" binding=\"name\" escapeLabel=\"false\" />"));
		assertThrows(SAXException.class,
						() -> validate("<chart xmlns=\"" + VIEW_NAMESPACE + "\" type=\"pie\" escapeTitle=\"false\" />"));
	}

	private static void validate(String xml) throws Exception {
		Path schemaPath = Path.of("src/test/resources/schemas/view.xsd");
		SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		Schema schema = factory.newSchema(schemaPath.toFile());
		Validator validator = schema.newValidator();
		validator.validate(new StreamSource(new StringReader(xml)));
	}
}
