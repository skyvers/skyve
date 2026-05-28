package modules.admin.Startup.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Startup.StartupExtension;

/**
 * Persists startup configuration changes to the override configuration file.
 */
public class SaveChanges implements ServerSideAction<StartupExtension> {
	/**
	 * Saves startup configuration updates.
	 *
	 * @param bean the startup bean containing edited values
	 * @param webContext the current web context
	 * @return the action result wrapping the startup bean
	 * @throws Exception if configuration persistence fails
	 */
	@Override
	public ServerSideActionResult<StartupExtension> execute(StartupExtension bean, WebContext webContext) throws Exception {

		// update the configuration
		bean.saveConfiguration();

		return new ServerSideActionResult<>(bean);
	}
}
