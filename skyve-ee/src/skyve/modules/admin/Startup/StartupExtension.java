package modules.admin.Startup;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.JSON;

import modules.admin.domain.Startup;

public class StartupExtension extends Startup {

	private static final long serialVersionUID = -8931459527432227257L;

	private static final String ENVIRONMENT_STANZA_KEY = "environment";
	private static final String ENVIRONMENT_IDENTIFIER_KEY = "identifier";
	private static final String ENVIRONMENT_SHOW_SETUP_KEY = "showSetup";
	private static final String ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY = "supportEmailAddress";

	public void saveConfiguration() throws IOException {
		if (Boolean.TRUE.equals(getDontShowAgain())) {
			UtilImpl.SHOW_SETUP = false;
		}

		Map<String, Object> properties = new HashMap<>();

		properties.put(ENVIRONMENT_STANZA_KEY, putEnvironment());

		String json = JSON.marshall(CORE.getCustomer(), properties);

		// write the json out to the content directory
		File overridesFile = new File(UtilImpl.CONTENT_DIRECTORY, UtilImpl.ARCHIVE_NAME + ".json");

		try (BufferedWriter writer = new BufferedWriter(new FileWriter(overridesFile))) {
			writer.write(json);
		}
	}

	private Map<String, Object> putEnvironment() {
		Map<String, Object> environment = new HashMap<>();

		if (!UtilImpl.ENVIRONMENT_IDENTIFIER.equals(getEnvironmentIdentifier())) {
			environment.put(ENVIRONMENT_IDENTIFIER_KEY, getEnvironmentIdentifier());
			UtilImpl.ENVIRONMENT_IDENTIFIER = getEnvironmentIdentifier();
		}

		if (!UtilImpl.SUPPORT_EMAIL_ADDRESS.equals(getEnvironmentSupportEmail())) {
			environment.put(ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY, getEnvironmentSupportEmail());
			UtilImpl.SUPPORT_EMAIL_ADDRESS = getEnvironmentSupportEmail();
		}

		if (Boolean.TRUE.equals(getDontShowAgain())) {
			environment.put(ENVIRONMENT_SHOW_SETUP_KEY, Boolean.FALSE);
		}

		return environment;
	}
}
