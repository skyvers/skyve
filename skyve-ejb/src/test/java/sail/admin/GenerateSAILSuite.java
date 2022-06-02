package sail.admin;

import java.io.File;
import java.io.FileOutputStream;
import java.util.List;

import org.skyve.CORE;
import org.skyve.impl.generate.sail.Generator;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.job.Job;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.UserAgentType;

import modules.admin.domain.User;

public class GenerateSAILSuite extends Job {
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
//		Path menuPath = Paths.get(String.format("%s%s%s", module_directory, File.separator, menuFileName));
		try (FileOutputStream menuOutputStream = new FileOutputStream(menuFileName)) {
			menuOutputStream.write(xmlMenuSail.getBytes());
		}
/*
		Automation moduleSail = Generator.visitMenu(user, moduleName, uxui.getName(), uat, testStrategy);
		String xmlModouleSail = XMLMetaData.marshalSAIL(moduleSail);
		String moduleFileName = String.format("sail_menu_%s_%s_%s.xml", uxui.getName(), uat.name(), user.getName());
		Path modulePath = Paths.get(String.format("%s%s%s", module_directory, File.separator, moduleFileName));
		try (FileOutputStream moduleOutputStream = new FileOutputStream(menuFileName)) {
			moduleOutputStream.write(xmlMenuSail.getBytes());
		}
*/
	}
}
