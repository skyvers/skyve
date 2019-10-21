package modules.admin.Startup.actions;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.JSON;
import org.skyve.web.WebContext;

import modules.admin.domain.Startup;

public class Dismiss implements ServerSideAction<Startup> {

	private static final long serialVersionUID = -2131639715230917502L;

	public static final String ENVIRONMENT_STANZA_KEY = "environment";
	private static final String ENVIRONMENT_SHOW_SETUP_KEY = "showSetup";

	@Override
	public ServerSideActionResult<Startup> execute(Startup bean, WebContext webContext) throws Exception {

		// check if the user ticket don't show again
		if (Boolean.TRUE.equals(bean.getDontShowAgain())) {
			// update the configuration
			UtilImpl.SHOW_SETUP = false;

			Map<String, Object> properties = new HashMap<>();
			Map<String, Object> environment = new HashMap<>();
			properties.put(ENVIRONMENT_STANZA_KEY, environment);

			environment.put(ENVIRONMENT_SHOW_SETUP_KEY, Boolean.FALSE);

			String json = JSON.marshall(CORE.getCustomer(), properties);

			// write the json out to the content directory
			File overridesFile = new File(UtilImpl.CONTENT_DIRECTORY, UtilImpl.ARCHIVE_NAME + ".json");

			try (BufferedWriter writer = new BufferedWriter(new FileWriter(overridesFile))) {
				writer.write(json);
			}
		}

		return new ServerSideActionResult<>(bean);
	}

}
