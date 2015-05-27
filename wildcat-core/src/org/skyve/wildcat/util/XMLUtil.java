package org.skyve.wildcat.util;

import static javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI;

import java.io.File;
import java.io.FileReader;
import java.io.StringWriter;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.skyve.metadata.MetaDataException;
import org.skyve.wildcat.metadata.repository.customer.CustomerMetaData;
import org.skyve.wildcat.metadata.repository.document.DocumentMetaData;
import org.skyve.wildcat.metadata.repository.module.ModuleMetaData;
import org.skyve.wildcat.metadata.repository.router.Router;
import org.skyve.wildcat.metadata.repository.view.ViewMetaData;
import org.xml.sax.SAXException;

public class XMLUtil {
	public static final String COMMON_NAMESPACE = "http://www.skyve.org/xml/common";
	public static final String ROUTER_NAMESPACE = "http://www.skyve.org/xml/router";
	public static final String CUSTOMER_NAMESPACE = "http://www.skyve.org/xml/customer";
	public static final String MODULE_NAMESPACE = "http://www.skyve.org/xml/module";
	public static final String DOCUMENT_NAMESPACE = "http://www.skyve.org/xml/document";
	public static final String VIEW_NAMESPACE = "http://www.skyve.org/xml/view";

	private static final JAXBContext ROUTER_CONTEXT;
	private static final Schema ROUTER_SCHEMA;

	private static final JAXBContext CUSTOMER_CONTEXT;
	private static final Schema CUSTOMER_SCHEMA;

	private static final JAXBContext MODULE_CONTEXT;
	private static final Schema MODULE_SCHEMA;

	private static final JAXBContext DOCUMENT_CONTEXT;
	private static final Schema DOCUMENT_SCHEMA;

	private static final JAXBContext VIEW_CONTEXT;
	private static final Schema VIEW_SCHEMA;

	static {
		try {
			ROUTER_CONTEXT = JAXBContext.newInstance(Router.class);
			ROUTER_SCHEMA = getSchema(UtilImpl.getAbsoluteBasePath() + "schemas/router.xsd");

			CUSTOMER_CONTEXT = JAXBContext.newInstance(CustomerMetaData.class);
			CUSTOMER_SCHEMA = getSchema(UtilImpl.getAbsoluteBasePath() + "schemas/customer.xsd");

			MODULE_CONTEXT = JAXBContext.newInstance(ModuleMetaData.class);
			MODULE_SCHEMA = getSchema(UtilImpl.getAbsoluteBasePath() + "schemas/module.xsd");

			DOCUMENT_CONTEXT = JAXBContext.newInstance(DocumentMetaData.class);
			DOCUMENT_SCHEMA = getSchema(UtilImpl.getAbsoluteBasePath() + "schemas/document.xsd");

			VIEW_CONTEXT = JAXBContext.newInstance(ViewMetaData.class);
			VIEW_SCHEMA = getSchema(UtilImpl.getAbsoluteBasePath() + "schemas/view.xsd");
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not initialize one of the metadata JAXB contexts", e);
		}
	}

	private XMLUtil() {
		// prevent construction
	}

	public static String marshalRouter(Router router) 
	throws MetaDataException {
		try {
			Marshaller marshaller = ROUTER_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									ROUTER_NAMESPACE + " ../schemas/router.xsd");
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(router, sos);
			return sos.toString();
		}
		catch (JAXBException e) {
			throw new MetaDataException("Could not marshal router", e);
		}
	}

	public static Router unmarshalRouter(String file) 
	throws MetaDataException {
		try (FileReader fr = new FileReader(file)) {
			Unmarshaller unmarshaller = ROUTER_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(ROUTER_SCHEMA);
			return (Router) unmarshaller.unmarshal(fr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal router at " + file, e);
		}
	}

	public static String marshalCustomer(CustomerMetaData customer) 
	throws MetaDataException {
		try {
			Marshaller marshaller = CUSTOMER_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									CUSTOMER_NAMESPACE + " ../../schemas/customer.xsd");
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(customer, sos);
			return sos.toString();
		}
		catch (JAXBException e) {
			throw new MetaDataException("Could not marshal customer " + customer.getName(), e);
		}
	}

	public static CustomerMetaData unmarshalCustomer(String file) 
	throws MetaDataException {
		try (FileReader fr = new FileReader(file)) {
			Unmarshaller unmarshaller = CUSTOMER_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(CUSTOMER_SCHEMA);
			return (CustomerMetaData) unmarshaller.unmarshal(fr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal customer at " + file, e);
		}
	}

	public static String marshalModule(ModuleMetaData module, boolean overridden) 
	throws MetaDataException {
		try {
			Marshaller marshaller = MODULE_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									(overridden ? 
										MODULE_NAMESPACE + " ../../../schemas/module.xsd" :
										MODULE_NAMESPACE + " ../../schemas/module.xsd"));
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(module, sos);
			return sos.toString();
		}
		catch (JAXBException e) {
			throw new MetaDataException("Could not marshal module " + module.getName(), e);
		}
	}

	public static ModuleMetaData unmarshalModule(String file) 
	throws MetaDataException {
		try (FileReader fr = new FileReader(file)) {
			Unmarshaller unmarshaller = MODULE_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(MODULE_SCHEMA);
			return (ModuleMetaData) unmarshaller.unmarshal(fr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal module at " + file, e);
		}
	}

	public static String marshalDocument(DocumentMetaData document, boolean overridden) 
	throws MetaDataException {
		try {
			Marshaller marshaller = DOCUMENT_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									(overridden ?
										DOCUMENT_NAMESPACE + " ../../../../schemas/document.xsd" :
										DOCUMENT_NAMESPACE + " ../../../schemas/document.xsd"));
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(document, sos);
			return sos.toString();
		}
		catch (JAXBException e) {
			throw new MetaDataException("Could not marshal document " + document.getName(), e);
		}
	}

	public static DocumentMetaData unmarshalDocument(String file) 
	throws MetaDataException {
		try (FileReader fr = new FileReader(file)) {
			Unmarshaller unmarshaller = DOCUMENT_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(DOCUMENT_SCHEMA);
			return (DocumentMetaData) unmarshaller.unmarshal(fr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal document at " + file, e);
		}
	}

	public static String marshalView(ViewMetaData view, boolean customerOverridden, boolean uxuiOverridden) 
	throws MetaDataException {
		try {
			Marshaller marshaller = VIEW_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			StringBuilder location = new StringBuilder(64);
			location.append(VIEW_NAMESPACE).append(' ');
			if (customerOverridden) {
				location.append("../../");
			}
			if (uxuiOverridden) {
				location.append("../");
			}
			location.append("../../../../schemas/view.xsd");
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, location.toString());
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(view, sos);
			return sos.toString();
		}
		catch (JAXBException e) {
			throw new MetaDataException("Could not marshal " + view.getType() + " view", e);
		}
	}

	public static ViewMetaData unmarshalView(String file) 
	throws MetaDataException {
		try (FileReader fr = new FileReader(file)) {
			Unmarshaller unmarshaller = VIEW_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(VIEW_SCHEMA);
			return (ViewMetaData) unmarshaller.unmarshal(fr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal view at " + file, e);
		}
	}

	/*
	 * returns a JAXP 1.3 schema by parsing the specified resource.
	 */
	private static Schema getSchema(String schemaFileName) 
	throws JAXBException {
		SchemaFactory sf = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI);
		try {
			return sf.newSchema(new File(schemaFileName));
		}
		catch (SAXException se) {
			// this can only happen if there's a deployment error and the resource is missing.
			throw new JAXBException("Could not find XML Schema for " + schemaFileName, se);
		}
	}
}
