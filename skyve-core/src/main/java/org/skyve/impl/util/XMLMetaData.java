package org.skyve.impl.util;

import static javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.dom4j.Attribute;
import org.dom4j.CDATA;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.dom4j.Visitor;
import org.dom4j.VisitorSupport;
import org.dom4j.io.SAXReader;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.util.Util;
import org.xml.sax.SAXException;

/**
 * Marshal and unmarshal XML.
 * 
 * Note:-
 * It should be possible to control the namespace prefixes generated in the XML with the JAXB RI implementation like this...
 *			marshaller.setProperty("com.sun.xml.internal.bind.namespacePrefixMapper", new NamespacePrefixMapper() {
 *				@Override
 *				public String getPreferredPrefix(String namespaceUri, String suggestion, boolean requirePrefix) {
 *					if (ROUTER_NAMESPACE.equals(namespaceUri)) {
 *						return "";
 *					}
 *					return suggestion;
 *				}
 *			});
 * but because it is class loaded by the module class loader in wildfly (via CXF) this class extension 
 * can't be seen and generates a linkage error.
 * 
 * The same holds for CharacterEscapeHandler.
 * 
 * Therefore we have to resort to post processing the output with DOM4J.
 */
public class XMLMetaData {
	public static final String COMMON_NAMESPACE = "http://www.skyve.org/xml/common";
	public static final String ROUTER_NAMESPACE = "http://www.skyve.org/xml/router";
	public static final String CUSTOMER_NAMESPACE = "http://www.skyve.org/xml/customer";
	public static final String MODULE_NAMESPACE = "http://www.skyve.org/xml/module";
	public static final String DOCUMENT_NAMESPACE = "http://www.skyve.org/xml/document";
	public static final String VIEW_NAMESPACE = "http://www.skyve.org/xml/view";
	public static final String SAIL_NAMESPACE = "http://www.skyve.org/xml/sail";
	public static final String CDATA_START_TAG = "<![CDATA[";
	public static final String CDATA_END_TAG = "]]>";
	public static final int CDATA_MIN_LENGTH = CDATA_START_TAG.length() + CDATA_END_TAG.length();
	
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

	private static final JAXBContext SAIL_CONTEXT;
	private static final Schema SAIL_SCHEMA;

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

			SAIL_CONTEXT = JAXBContext.newInstance(Automation.class);
			SAIL_SCHEMA = getSchema(UtilImpl.getAbsoluteBasePath() + "schemas/sail.xsd");
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not initialize one of the metadata JAXB contexts", e);
		}
	}

	private XMLMetaData() {
		// prevent construction
	}

	public static String marshalRouter(Router router) {
		try {
			Marshaller marshaller = ROUTER_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									ROUTER_NAMESPACE + " ../schemas/router.xsd");
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(router, sos);

			Document document = new SAXReader().read(new StringReader(sos.toString()));
			Visitor visitor = new JAXBFixingVisitor(ROUTER_NAMESPACE);
			document.accept(visitor);
			return document.asXML();
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal router", e);
		}
	}

	public static Router unmarshalRouterFile(String file) {
		// NB Cannot use FileReader in here as it doesn't work with UTF-8 properly on linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		File f = new File(file);
		try (FileInputStream fis = new FileInputStream(f)) {
			try (BufferedInputStream bis = new BufferedInputStream(fis)) {
				try (InputStreamReader isr = new InputStreamReader(bis, Util.UTF8)) {
					try (BufferedReader br = new BufferedReader(isr)) {
						Unmarshaller unmarshaller = ROUTER_CONTEXT.createUnmarshaller();
						unmarshaller.setSchema(ROUTER_SCHEMA);
						Router result = (Router) unmarshaller.unmarshal(br);
						result.setLastModifiedMillis(f.lastModified());
						return result;
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal router at " + file, e);
		}
	}

	public static Router unmarshalRouterString(String xml) {
		try (StringReader sr = new StringReader(xml)) {
			Unmarshaller unmarshaller = ROUTER_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(ROUTER_SCHEMA);
			return (Router) unmarshaller.unmarshal(sr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal router xml " + xml, e);
		}
	}

	public static String marshalCustomer(CustomerMetaData customer) {
		try {
			Marshaller marshaller = CUSTOMER_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									CUSTOMER_NAMESPACE + " ../../schemas/customer.xsd");
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(customer, sos);

			Document document = new SAXReader().read(new StringReader(sos.toString()));
			Visitor visitor = new JAXBFixingVisitor(CUSTOMER_NAMESPACE);
			document.accept(visitor);

			String xml = cleanup(document.asXML());
			return xml;
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal customer " + customer.getName(), e);
		}
	}

	/**
	 * Writes the CustomerMetaData to a new customer.xml in the location of the
	 * specified directory. This will overwrite any existing file in that location with
	 * the same name as specified in the metadata.
	 *
	 * @param customer The customer to output to a file
	 * @param sourceDirectory The root source directory, e.g. <code>src/main/java</code>
	 */
	public static void marshalCustomer(CustomerMetaData customer, String sourceDirectory) {
		// NB Cannot use FileWriter in here as it doesn't work with UTF-8 properly on Linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		StringBuilder filePath = new StringBuilder(64);
		filePath.append(sourceDirectory);
		if (!sourceDirectory.endsWith("/") && !sourceDirectory.endsWith("\\")) {
			filePath.append('/');
		}
		filePath.append("customers/").append(customer.getName()).append('/');
		File file = new File(filePath.toString());
		file.mkdirs();
		filePath.append(customer.getName()).append(".xml");
		file = new File(filePath.toString());
		Util.LOGGER.info(String.format("Attempting to write %s.xml to %s", customer.getName(), file.getAbsolutePath()));

		try (FileOutputStream fos = new FileOutputStream(file)) {
			try (BufferedOutputStream bos = new BufferedOutputStream(fos)) {
				try (OutputStreamWriter osw = new OutputStreamWriter(bos, Util.UTF8)) {
					try (BufferedWriter bw = new BufferedWriter(osw)) {
						String contents = marshalCustomer(customer);
						bw.write(contents);
						bw.flush();
					}
				}
			}
		} 
		catch (Exception e) {
			throw new MetaDataException("Could not marshal customer at " + file.getPath(), e);
		}
	}

	public static CustomerMetaData unmarshalCustomerFile(String file) {
		// NB Cannot use FileReader in here as it doesn't work with UTF-8 properly on linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		File f = new File(file);
		try (FileInputStream fis = new FileInputStream(f)) {
			try (BufferedInputStream bis = new BufferedInputStream(fis)) {
				try (InputStreamReader isr = new InputStreamReader(bis, Util.UTF8)) {
					try (BufferedReader br = new BufferedReader(isr)) {
						Unmarshaller unmarshaller = CUSTOMER_CONTEXT.createUnmarshaller();
						unmarshaller.setSchema(CUSTOMER_SCHEMA);
						CustomerMetaData result = (CustomerMetaData) unmarshaller.unmarshal(br);
						result.setLastModifiedMillis(f.lastModified());
						return result;
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal customer at " + file, e);
		}
	}

	public static CustomerMetaData unmarshalCustomerString(String xml) {
		try (StringReader sr = new StringReader(xml)) {
			Unmarshaller unmarshaller = CUSTOMER_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(CUSTOMER_SCHEMA);
			return (CustomerMetaData) unmarshaller.unmarshal(sr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal customer xml " + xml, e);
		}
	}

	public static String marshalModule(ModuleMetaData module, boolean overridden) {
		try {
			Marshaller marshaller = MODULE_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									(overridden ? 
										MODULE_NAMESPACE + " ../../../schemas/module.xsd" :
										MODULE_NAMESPACE + " ../../schemas/module.xsd"));
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(module, sos);

			Document document = new SAXReader().read(new StringReader(sos.toString()));
			Visitor visitor = new JAXBFixingVisitor(MODULE_NAMESPACE);
			document.accept(visitor);
			
			String xml = cleanup(document.asXML());
			return xml;
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal module " + module.getName(), e);
		}
	}

	/**
	 * Writes the ModuleMetaData to a new module.xml in the location of the
	 * specified directory. This will overwrite any existing file in that location with
	 * the same name as specified in the metadata.
	 * 
	 * The module will create it's own package directory if it doesn't already exist,
	 * and create a module.xml according to the <code>name</code> specified in the
	 * ModuleMetaData.
	 * 
	 * @param module The module to output to a file
	 * @param overridden Should be true if this module is a customer override, false otherwise
	 * @param modulesDirectory The root source modules directory, e.g. <code>src/main/java/modules</code>
	 */
	public static void marshalModule(ModuleMetaData module, boolean overridden, String modulesDirectory) {
		// NB Cannot use FileWriter in here as it doesn't work with UTF-8 properly on Linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		StringBuilder filePath = new StringBuilder(64);
		filePath.append(modulesDirectory);
		if (!modulesDirectory.endsWith("/") && !modulesDirectory.endsWith("\\")) {
			filePath.append('/');
		}
		filePath.append(module.getName()).append('/');
		File file = new File(filePath.toString());
		file.mkdirs();
		filePath.append(module.getName()).append(".xml");
		file = new File(filePath.toString());
		Util.LOGGER.info(String.format("Attempting to write module.xml to %s", file.getAbsolutePath()));

		try (FileOutputStream fos = new FileOutputStream(file)) {
			try (BufferedOutputStream bos = new BufferedOutputStream(fos)) {
				try (OutputStreamWriter osw = new OutputStreamWriter(bos, Util.UTF8)) {
					try (BufferedWriter bw = new BufferedWriter(osw)) {
						String contents = marshalModule(module, overridden);
						bw.write(contents);
						bw.flush();
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal document at " + file.getPath(), e);
		}
	}

	public static ModuleMetaData unmarshalModuleFile(String file) {
		// NB Cannot use FileReader in here as it doesn't work with UTF-8 properly on linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		File f = new File(file);
		try (FileInputStream fis = new FileInputStream(f)) {
			try (BufferedInputStream bis = new BufferedInputStream(fis)) {
				try (InputStreamReader isr = new InputStreamReader(bis, Util.UTF8)) {
					try (BufferedReader br = new BufferedReader(isr)) {
						Unmarshaller unmarshaller = MODULE_CONTEXT.createUnmarshaller();
						unmarshaller.setSchema(MODULE_SCHEMA);
						ModuleMetaData result = (ModuleMetaData) unmarshaller.unmarshal(br);
						result.setLastModifiedMillis(f.lastModified());
						return result;
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal module at " + file, e);
		}
	}

	public static ModuleMetaData unmarshalModuleString(String xml) {
		try (StringReader sr = new StringReader(xml)) {
			Unmarshaller unmarshaller = MODULE_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(MODULE_SCHEMA);
			return (ModuleMetaData) unmarshaller.unmarshal(sr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal module xml " + xml, e);
		}
	}

	public static String marshalDocument(DocumentMetaData document, boolean overridden) {
		try {
			Marshaller marshaller = DOCUMENT_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
					(overridden ? DOCUMENT_NAMESPACE + " ../../../../schemas/document.xsd"
							: DOCUMENT_NAMESPACE + " ../../../schemas/document.xsd"));
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(document, sos);

			Document doc = new SAXReader().read(new StringReader(sos.toString()));
			Visitor visitor = new JAXBFixingVisitor(DOCUMENT_NAMESPACE);
			doc.accept(visitor);

			String xml = cleanup(doc.asXML());
			return xml;
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal document " + document.getName(), e);
		}
	}

	/**
	 * Writes the DocumentMetaData to a new document.xml in the location of the
	 * specified file. This will overwrite any existing file in that location with
	 * the same name as specified in the metadata.
	 * 
	 * The document will create it's own package directory if it doesn't already exist,
	 * and create a document.xml according to the <code>name</code> specified in the
	 * DocumentMetaData.
	 * 
	 * @param document The document to output to a file
	 * @param overridden Should be true if this document is a customer override, false otherwise
	 * @param documentModuleDirectory The path to the module this document belongs to
	 */
	public static void marshalDocument(DocumentMetaData document, boolean overridden, String documentModuleDirectory) {
		// NB Cannot use FileWriter in here as it doesn't work with UTF-8 properly on Linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		StringBuilder filePath = new StringBuilder(64);
		filePath.append(documentModuleDirectory);
		if (!documentModuleDirectory.endsWith("/") && !documentModuleDirectory.endsWith("\\")) {
			filePath.append('/');
		}
		filePath.append(document.getName()).append('/');
		File file = new File(filePath.toString());
		file.mkdirs();
		filePath.append(document.getName()).append(".xml");
		file = new File(filePath.toString());
		Util.LOGGER.info(String.format("Attempting to write document.xml to %s", file.getPath()));

		try (FileOutputStream fos = new FileOutputStream(file)) {
			try (BufferedOutputStream bos = new BufferedOutputStream(fos)) {
				try (OutputStreamWriter osw = new OutputStreamWriter(bos, Util.UTF8)) {
					try (BufferedWriter bw = new BufferedWriter(osw)) {
						String contents = marshalDocument(document, overridden);
						bw.write(contents);
						bw.flush();
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal document at " + file.getPath(), e);
		}
	}

	public static DocumentMetaData unmarshalDocumentFile(String file) {
		// NB Cannot use FileReader in here as it doesn't work with UTF-8 properly on linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		File f = new File(file);
		try (FileInputStream fis = new FileInputStream(f)) {
			try (BufferedInputStream bis = new BufferedInputStream(fis)) {
				try (InputStreamReader isr = new InputStreamReader(bis, Util.UTF8)) {
					try (BufferedReader br = new BufferedReader(isr)) {
						Unmarshaller unmarshaller = DOCUMENT_CONTEXT.createUnmarshaller();
						unmarshaller.setSchema(DOCUMENT_SCHEMA);
						DocumentMetaData result = (DocumentMetaData) unmarshaller.unmarshal(br);
						result.setLastModifiedMillis(f.lastModified());
						return result;
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal document at " + file, e);
		}
	}

	public static DocumentMetaData unmarshalDocumentString(String xml) {
		try (StringReader sr = new StringReader(xml)) {
			Unmarshaller unmarshaller = DOCUMENT_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(DOCUMENT_SCHEMA);
			return (DocumentMetaData) unmarshaller.unmarshal(sr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal document xml " + xml, e);
		}
	}

	public static String marshalView(ViewMetaData view, boolean customerOverridden, boolean uxuiOverridden) {
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

			Document document = new SAXReader().read(new StringReader(sos.toString()));
			Visitor visitor = new JAXBFixingVisitor(VIEW_NAMESPACE);
			document.accept(visitor);
			return document.asXML();
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal " + view.getName() + " view", e);
		}
	}

	/**
	 * Writes the ViewMetaData to a new view.xml in the location of the
	 * specified file. This will overwrite any existing file in that location with
	 * the same name as specified in the metadata.
	 * 
	 * The view will create it's own package directory if it doesn't already exist,
	 * and create a view.xml according to the <code>name</code> specified in the
	 * ViewMetaData.
	 * 
	 * @param view The view to output to a file
	 * @param customerOverridden Should be true if this view is a customer override, false otherwise
	 * @param uxuiOverridden Should be true if this view is an Ux/UI override, false otherwise
	 * @param viewDocumentDirectory The path to the document this view belongs to
	 */
	public static void marshalView(ViewMetaData view, boolean customerOverridden, boolean uxuiOverridden, String viewDocumentDirectory) {
		// NB Cannot use FileWriter in here as it doesn't work with UTF-8 properly on Linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		StringBuilder filePath = new StringBuilder(64);
		filePath.append(viewDocumentDirectory);
		if ((! viewDocumentDirectory.endsWith("/")) && (! viewDocumentDirectory.endsWith("\\"))) {
			filePath.append('/');
		}
		filePath.append("views/");
		File file = new File(filePath.toString());
		file.mkdirs();
		filePath.append(view.getName()).append(".xml");
		file = new File(filePath.toString());
		Util.LOGGER.info(String.format("Attempting to write view.xml to %s", file.getPath()));

		try (FileOutputStream fos = new FileOutputStream(file)) {
			try (BufferedOutputStream bos = new BufferedOutputStream(fos)) {
				try (OutputStreamWriter osw = new OutputStreamWriter(bos, Util.UTF8)) {
					try (BufferedWriter bw = new BufferedWriter(osw)) {
						String contents = marshalView(view, customerOverridden, uxuiOverridden);
						bw.write(contents);
						bw.flush();
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal document at " + file.getPath(), e);
		}
	}
	
	public static ViewMetaData unmarshalViewFile(String file) {
		// NB Cannot use FileReader in here as it doesn't work with UTF-8 properly on linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		File f = new File(file);
		try (FileInputStream fis = new FileInputStream(f)) {
			try (BufferedInputStream bis = new BufferedInputStream(fis)) {
				try (InputStreamReader isr = new InputStreamReader(bis, Util.UTF8)) {
					try (BufferedReader br = new BufferedReader(isr)) {
						Unmarshaller unmarshaller = VIEW_CONTEXT.createUnmarshaller();
						unmarshaller.setSchema(VIEW_SCHEMA);
						ViewMetaData result = (ViewMetaData) unmarshaller.unmarshal(br);
						result.setLastModifiedMillis(f.lastModified());
						return result;
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal view at " + file, e);
		}
	}

	public static ViewMetaData unmarshalViewString(String xml) {
		try (StringReader sr = new StringReader(xml)) {
			Unmarshaller unmarshaller = VIEW_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(VIEW_SCHEMA);
			return (ViewMetaData) unmarshaller.unmarshal(sr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal view xml " + xml, e);
		}
	}

	public static String marshalSAIL(Automation automation) {
		try {
			Marshaller marshaller = SAIL_CONTEXT.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
									SAIL_NAMESPACE + " http://www.skyve.org/xml/sail.xsd");
			marshaller.setSchema(SAIL_SCHEMA);
			StringWriter sos = new StringWriter(1024);
			marshaller.marshal(automation, sos);

			Document document = new SAXReader().read(new StringReader(sos.toString()));
			Visitor visitor = new JAXBFixingVisitor(VIEW_NAMESPACE);
			document.accept(visitor);
			return document.asXML();
		}
		catch (Exception e) {
			throw new MetaDataException("Could not marshal SAIL", e);
		}
	}

	public static Automation unmarshalSAILFile(String file) {
		// NB Cannot use FileReader in here as it doesn't work with UTF-8 properly on linux.
		// We need to specifically mention UTF-8 to get this to happen in the adapter abomination below
		try (FileInputStream fis = new FileInputStream(file)) {
			try (BufferedInputStream bis = new BufferedInputStream(fis)) {
				try (InputStreamReader isr = new InputStreamReader(bis, Util.UTF8)) {
					try (BufferedReader br = new BufferedReader(isr)) {
						Unmarshaller unmarshaller = SAIL_CONTEXT.createUnmarshaller();
						unmarshaller.setSchema(SAIL_SCHEMA);
						return (Automation) unmarshaller.unmarshal(br);
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal SAIL at " + file, e);
		}
	}

	public static Automation unmarshalSAILString(String sail) {
		try (StringReader sr = new StringReader(sail)) {
			Unmarshaller unmarshaller = SAIL_CONTEXT.createUnmarshaller();
			unmarshaller.setSchema(SAIL_SCHEMA);
			return (Automation) unmarshaller.unmarshal(sr);
		}
		catch (Exception e) {
			throw new MetaDataException("Could not unmarshal SAIL " + sail, e);
		}
	}

	/**
	 * Cleans the XML string by removing any empty lines left by removing
	 * nodes during the {@link JAXBFixingVisitor}.
	 * 
	 * @param xml The xml to clean
	 * @return The cleansed string
	 */
	private static String cleanup(String xml) {
		return xml != null ? xml.replaceAll("\n\\s{4,}\n", "\n") : null;
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

	/**
	 * Visitor to fix JAXB namespace issues, remove empty elements with no children,
	 * and optional attributes set to their default value.
	 */
	private static class JAXBFixingVisitor extends VisitorSupport {
		private static final Namespace COMMON = Namespace.get("c", COMMON_NAMESPACE);
		private static final Namespace MODULE = Namespace.get("m", MODULE_NAMESPACE);
		private static final Namespace DOCUMENT = Namespace.get("d", DOCUMENT_NAMESPACE);
		private static final Namespace VIEW = Namespace.get("v", VIEW_NAMESPACE);

		private Namespace target;
		private String targetUri;

		JAXBFixingVisitor(String targetNamespaceUri) {
			target = Namespace.get("", targetNamespaceUri);
			targetUri = targetNamespaceUri;
		}

		@Override
		public void visit(Element node) {
			Namespace ns = node.getNamespace();

			String uri = ns.getURI();
			if (uri.equals(targetUri)) {
				QName newQName = new QName(node.getName(), target);
				node.setQName(newQName);
			}
			else if (uri.equals(COMMON_NAMESPACE)) {
				QName newQName = new QName(node.getName(), COMMON);
				node.setQName(newQName);
			}
			else if (uri.equals(MODULE_NAMESPACE)) {
				QName newQName = new QName(node.getName(), MODULE);
				node.setQName(newQName);
			}
			else if (uri.equals(DOCUMENT_NAMESPACE)) {
				QName newQName = new QName(node.getName(), DOCUMENT);
				node.setQName(newQName);
			}
			else if (uri.equals(VIEW_NAMESPACE)) {
				QName newQName = new QName(node.getName(), VIEW);
				node.setQName(newQName);
			}

			// detect any empty customer elements which require children
			if (uri.equals(CUSTOMER_NAMESPACE)) {
				if (node.getParent() == null) {
					removeEmptyChildElements(node, new String[] { "interceptors", "observers" });
				}
			}

			// detect any empty module elements which require children
			if (uri.equals(MODULE_NAMESPACE)) {
				if (node.getParent() == null) {
					removeEmptyChildElements(node, new String[] { "jobs", "queries", "privileges" });
				}
			}

			// detect any empty document elements which require children
			if (uri.equals(DOCUMENT_NAMESPACE)) {
				Element parent = node.getParent();
				if (parent == null) {
					removeEmptyChildElements(node, new String[] { "conditions", "implements", "uniqueConstraints" });
				}

				if (parent != null && parent.getName().equals("attributes")) {
					if (node.attributeCount() > 0) {
						Map<String, Boolean> attributesToRemove = new HashMap<>();
						attributesToRemove.put("deprecated", Boolean.FALSE);
						attributesToRemove.put("persistent", Boolean.TRUE);
						attributesToRemove.put("required", Boolean.FALSE);

						removeDefaultAttributes(node, attributesToRemove);
					}
					
					// remove transient element from attributes where transient is false
					ListIterator<?> childNodes = node.elements().listIterator();
					while (childNodes.hasNext()) {
						Element child = (Element) childNodes.next();
						if ("transient".equals(child.getName()) && "false".equals(child.getText())) {
							childNodes.remove();
						}
					}
				}
			}
			
			ListIterator<?> namespaces = node.additionalNamespaces().listIterator();
			while (namespaces.hasNext()) {
				Namespace additionalNamespace = (Namespace) namespaces.next();
				String additionalNamespaceUri = additionalNamespace.getURI();
				if (additionalNamespaceUri.equals(COMMON_NAMESPACE)) {
					namespaces.remove();
				}
				else if (additionalNamespaceUri.equals(MODULE_NAMESPACE)) {
					namespaces.remove();
				}
				else if (additionalNamespaceUri.equals(DOCUMENT_NAMESPACE)) {
					namespaces.remove();
				}
				else if (additionalNamespaceUri.equals(VIEW_NAMESPACE)) {
					namespaces.remove();
				}
			}

			// Replace escaped characters within CDATA tags
			String text = Util.processStringValue(node.getText());
			if (text != null) {
				if (text.startsWith(CDATA_START_TAG) && text.endsWith(CDATA_END_TAG)) {
					text = text.substring(CDATA_START_TAG.length(), text.length() - CDATA_END_TAG.length());
					text = text.replace("&amp;", "&")
								.replace("&quot;", "\"")
								.replace("&lt;", "<")
								.replace("&gt;", ">");
					CDATA cdata = DocumentHelper.createCDATA(text);
					node.clearContent();
					node.add(cdata);
				}
			}
		}

		private static void removeDefaultAttributes(Element node, Map<String, Boolean> attributesToRemove) {
			Iterator<?> attributes = node.attributes().iterator();
			while (attributes.hasNext()) {
				Attribute a = (Attribute) attributes.next();

				if (attributesToRemove.keySet().contains(a.getName())) {
					if (Boolean.valueOf(a.getValue()).equals(attributesToRemove.get(a.getName()))) {
						attributes.remove();
					}
				}
			}
		}

		private static void removeEmptyChildElements(Element parent, String[] nodesToRemove) {
			List<String> nodesToRemoveList = Arrays.asList(nodesToRemove);
			
			ListIterator<?> childNodes = parent.elements().listIterator();
			while (childNodes.hasNext()) {
				Element child = (Element) childNodes.next();

				if (nodesToRemoveList.contains(child.getName())) {
					if (child.isTextOnly() && child.elements().size() == 0) {
						childNodes.remove();
					}
				}
			}
		}
	}
	
	public static void main(String[] args) throws Exception {
		JAXBContext jaxbContext = JAXBContext.newInstance(CustomerMetaData.class, 
															ModuleMetaData.class,
															DocumentMetaData.class,
															ViewMetaData.class,
															Router.class,
															Automation.class);
		jaxbContext.generateSchema(new SchemaOutputResolver() {
			@Override
			public Result createOutput(String namespaceUri, String suggestedFileName) throws IOException {
				File file = null;
				if (namespaceUri.endsWith("/common")) {
					file = new File("common.xsd");
				}
				else if (namespaceUri.endsWith("/customer")) {
					file = new File("customer.xsd");
				}
				else if (namespaceUri.endsWith("/module")) {
					file = new File("module.xsd");
				}
				else if (namespaceUri.endsWith("/document")) {
					file = new File("document.xsd");
				}
				else if (namespaceUri.endsWith("/view")) {
					file = new File("view.xsd");
				}
				else if (namespaceUri.endsWith("/router")) {
					file = new File("router.xsd");
				}
				else if (namespaceUri.endsWith("/sail")) {
					file = new File("sail.xsd");
				}
				else {
					throw new IllegalArgumentException(namespaceUri + " not catered for");
				}
		        StreamResult result = new StreamResult(file);
		        result.setSystemId(file.toURI().toURL().toString());
		        return result;
			}
		});
	}
}
