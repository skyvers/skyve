package org.skyve.impl.tools.test.wail;

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

import org.skyve.impl.tools.test.wail.language.TestSuite;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;

public class XMLUtil {
	public static final String WAIL_NAMESPACE = "http://www.skyve.org/xml/wail";
	private static final JAXBContext WAIL_CONTEXT;
	private static final Schema WAIL_SCHEMA;

	static {
		try {
			WAIL_CONTEXT = JAXBContext.newInstance(TestSuite.class);
			SchemaFactory sf = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI);
			WAIL_SCHEMA = sf.newSchema(new File(UtilImpl.getAbsoluteBasePath() + "schemas/wail/wail.xsd"));
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not initialize one of the metadata JAXB contexts", e);
		}
	}

	private XMLUtil() {
		// prevent construction
	}

	public static String marshalWAIL(TestSuite testSuite) 
	throws MetaDataException {
		try {
			Marshaller marshaller = WAIL_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									WAIL_NAMESPACE + " http://www.skyve.org/xml/wail.xsd");
			marshaller.setSchema(WAIL_SCHEMA);
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(testSuite, sos);
			return sos.toString();
		}
		catch (JAXBException e) {
			throw new MetaDataException("Could not marshal WAIL", e);
		}
	}

	public static TestSuite unmarshalWAIL(Reader xmlReader) 
	throws MetaDataException {
		try {
			Unmarshaller unmarshaller = WAIL_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(WAIL_SCHEMA);
			return (TestSuite) unmarshaller.unmarshal(xmlReader);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal WAIL", e);
		}
	}
}
