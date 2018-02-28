package org.skyve.impl.tools.test.sail;

import static javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI;

import java.io.File;
import java.io.Reader;
import java.io.StringWriter;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.skyve.impl.tools.test.sail.language.TestSuite;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;

public class XMLUtil {
	public static final String SAIL_NAMESPACE = "http://www.skyve.org/xml/sail";
	private static final JAXBContext SAIL_CONTEXT;
	private static final Schema SAIL_SCHEMA;

	static {
		try {
			SAIL_CONTEXT = JAXBContext.newInstance(TestSuite.class);
			SchemaFactory sf = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI);
			SAIL_SCHEMA = sf.newSchema(new File(UtilImpl.getAbsoluteBasePath() + "schemas/sail.xsd"));
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not initialize one of the metadata JAXB contexts", e);
		}
	}

	private XMLUtil() {
		// prevent construction
	}

	public static String marshalSAIL(TestSuite testSuite) {
		try {
			Marshaller marshaller = SAIL_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									SAIL_NAMESPACE + " http://www.skyve.org/xml/sail.xsd");
			marshaller.setSchema(SAIL_SCHEMA);
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(testSuite, sos);
			return sos.toString();
		}
		catch (JAXBException e) {
			throw new MetaDataException("Could not marshal SAIL", e);
		}
	}

	public static TestSuite unmarshalSAIL(Reader xmlReader) {
		try {
			Unmarshaller unmarshaller = SAIL_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(SAIL_SCHEMA);
			return (TestSuite) unmarshaller.unmarshal(xmlReader);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal SAIL", e);
		}
	}
}
