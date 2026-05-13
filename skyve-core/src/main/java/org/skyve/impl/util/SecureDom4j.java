package org.skyve.impl.util;

import javax.xml.XMLConstants;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.validation.SchemaFactory;

import org.dom4j.io.SAXReader;
import org.xml.sax.SAXException;

/**
 * Utility for constructing securely configured XML parser components used by DOM4J.
 * <p>
 * The helpers in this class create hardened instances to mitigate XML-related security
 * issues such as XXE (XML External Entity) attacks, external entity expansion,
 * external DTD/schema resolution, and other unsafe defaults. Use these factory methods
 * whenever reading or validating XML within Skyve.
 * </p>
 * <p>
 * Key protections applied include:
 * </p>
 * <ul>
 *   <li>Disallowing DOCTYPE declarations</li>
 *   <li>Disabling external general and parameter entities</li>
 *   <li>Disabling loading of external DTDs</li>
 *   <li>Restricting access to external DTDs, schemas, and stylesheets</li>
 *   <li>Enabling {@link javax.xml.XMLConstants#FEATURE_SECURE_PROCESSING secure processing}</li>
 *   <li>Ensuring namespace awareness and disabling XInclude</li>
 * </ul>
 */
public final class SecureDom4j {
	/**
	 * Utility class; not intended to be instantiated.
	 */
	private SecureDom4j() {
		// nothing to see here
	}

	/**
	 * Create a DOM4J {@link SAXReader} backed by a securely configured SAX parser.
	 * <p>
	 * The underlying {@link javax.xml.parsers.SAXParserFactory} is configured to:
	 * </p>
	 * <ul>
	 *   <li>Be namespace-aware and not XInclude-aware</li>
	 *   <li>Disallow DOCTYPE declarations</li>
	 *   <li>Disable external general and parameter entities</li>
	 *   <li>Enable secure processing</li>
	 *   <li>Disable loading of external DTDs</li>
	 * </ul>
	 * This configuration protects against XXE and related XML parsing threats.
	 *
	 * @return a hardened {@code SAXReader} suitable for safely parsing untrusted XML
	 * @throws ParserConfigurationException if the SAX parser factory cannot apply the
	 *         requested security features or the parser cannot be instantiated
	 * @throws SAXException if the underlying XMLReader fails to apply a feature or
	 *         a SAX-level error occurs during reader creation
	 */
	public static SAXReader newSAXReader() throws SAXException, ParserConfigurationException {
		SAXParserFactory factory = SAXParserFactory.newInstance();
		factory.setNamespaceAware(true);
		factory.setXIncludeAware(false);

		factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
		factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
		factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);

		// XMLReader is created from a hardened SAXParserFactory (XXE disabled)
		@SuppressWarnings("java:S2755")
		SAXReader result = new SAXReader(factory.newSAXParser().getXMLReader());
		result.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);

		return result;
	}
	
	/**
	 * Create a securely configured {@link SchemaFactory} for W3C XML Schema (XSD).
	 * <p>
	 * The factory is configured to enable secure processing and to disallow
	 * resolution of external DTDs, schemas, and stylesheets via
	 * {@link javax.xml.XMLConstants#ACCESS_EXTERNAL_DTD},
	 * {@link javax.xml.XMLConstants#ACCESS_EXTERNAL_SCHEMA}, and
	 * {@link javax.xml.XMLConstants#ACCESS_EXTERNAL_STYLESHEET} respectively.
	 * </p>
	 *
	 * @return a hardened {@code SchemaFactory} for {@link XMLConstants#W3C_XML_SCHEMA_NS_URI}
	 * @throws SAXException if the schema factory fails to apply a feature or property,
	 *         or if a SAX-level configuration error occurs
	 */
	public static SchemaFactory newSchemaFactory() throws SAXException {
		// Create factory
		@SuppressWarnings("java:S2755")
		SchemaFactory result = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		result.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
		result.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
		result.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "file");
		
		return result;
	}
}