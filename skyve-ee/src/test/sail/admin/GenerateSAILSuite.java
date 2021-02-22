package sail.admin;

import static javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI;

import java.io.File;
import java.io.FileOutputStream;
import java.io.StringWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.skyve.CORE;
import org.skyve.impl.generate.sail.Generator;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.job.Job;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.UserAgentType;
import org.xml.sax.SAXException;

import modules.admin.domain.User;

public class GenerateSAILSuite extends Job {

	/**
	 * 
	 */
	private static final long serialVersionUID = -783434573979058287L;
	
	
	private static String baseSailXMLPath = "C:/_/skyve/skyve-ee/src/test/sail/";

	@Override
	public void execute() throws Exception {

		TestStrategy testStrategy = TestStrategy.Verify;
		Router r = CORE.getRepository().getRouter();
		Repository repo = CORE.getRepository();

		// iterate through each combination of user, module, uxui and deviceagent
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		List<User> users = q.beanResults();

		for (User user : users) {

			org.skyve.metadata.user.User u = repo.retrieveUser(String.format("%s/%s", user.getBizCustomer(), user.getUserName()));

			for (UxUiMetadata uxui : r.getUxUis()) {

				// skip desktop
				if (!"desktop".equals(uxui.getName())) {

					for (UserAgentType uat : UserAgentType.values()) {
						switch (uat) {
						case desktop:
						case other:
							// do nothing
							break;
						case phone:
						case tablet:
							generateSAIL(u, "admin", uxui, uat, testStrategy);
							break;
						default:
							break;
						}
					}
				}
			}
		}
	}

	private static void generateSAIL(org.skyve.metadata.user.User user, String moduleName, UxUiMetadata uxui, UserAgentType uat, TestStrategy testStrategy)
			throws Exception {

		// check that the module folder exists, and if not, create it
		File module_directory = new File(String.format("%s%s", baseSailXMLPath, moduleName));
		if (!module_directory.exists()) {
			module_directory.mkdir();
		}

		// generate the SAIL XML to a named file
		Automation menuSail = Generator.visitMenu(user, moduleName, uxui.getName(), uat, testStrategy);
		String xmlMenuSail = XMLMetaData.marshalSAIL(menuSail);
		String menuFileName = String.format("sail_menu_%s_%s_%s.xml", uxui.getName(), uat.name(), user.getName());
		Path menuPath = Paths.get(String.format("%s%s%s", module_directory, File.separator, menuFileName));
		FileOutputStream menuOutputStream = new FileOutputStream(menuFileName);
		menuOutputStream.write(xmlMenuSail.getBytes());
		menuOutputStream.close();

		Automation moduleSail = Generator.visitMenu(user, moduleName, uxui.getName(), uat, testStrategy);
		String xmlModouleSail = XMLMetaData.marshalSAIL(moduleSail);
		String moduleFileName = String.format("sail_menu_%s_%s_%s.xml", uxui.getName(), uat.name(), user.getName());
		Path modulePath = Paths.get(String.format("%s%s%s", module_directory, File.separator, moduleFileName));
		FileOutputStream moduleOutputStream = new FileOutputStream(menuFileName);
		moduleOutputStream.write(xmlMenuSail.getBytes());
		moduleOutputStream.close();

	}

	
	private static String marshalSAIL(Automation automation) throws Exception{
		JAXBContext SAIL_CONTEXT = JAXBContext.newInstance(Automation.class);
		Schema SAIL_SCHEMA = getSchema(UtilImpl.getAbsoluteBasePath() + "schemas/sail.xsd");
		String SAIL_NAMESPACE = "http://www.skyve.org/xml/sail";
		
		Marshaller marshaller = SAIL_CONTEXT.createMarshaller();
		marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
		marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION,
								SAIL_NAMESPACE + " http://www.skyve.org/xml/sail.xsd");
		marshaller.setSchema(SAIL_SCHEMA);
		StringWriter sos = new StringWriter(1024);
		marshaller.marshal(automation, sos);
		return sos.toString();
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
