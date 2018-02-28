package org.skyve.impl.tools;

import java.io.File;
import java.io.IOException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.tools.test.sail.language.TestSuite;

public class XMLSchemaGenerator {
	public static void main(String[] args) throws Exception {
		JAXBContext jaxbContext = JAXBContext.newInstance(CustomerMetaData.class, 
															ModuleMetaData.class,
															DocumentMetaData.class,
															ViewMetaData.class,
															Router.class,
															TestSuite.class);
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
