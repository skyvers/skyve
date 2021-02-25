package org.skyve.addin;

import java.nio.file.Paths;
import java.util.List;

import org.pf4j.DefaultPluginManager;
import org.pf4j.PluginManager;
import org.pf4j.PluginWrapper;
import org.skyve.impl.util.UtilImpl;

public class DefaultAddInManager implements AddInManager {
	private static final DefaultAddInManager INSTANCE = new DefaultAddInManager();

	private PluginManager plugInManager;

	private DefaultAddInManager() {
		// nothing to see here
	}

	public static DefaultAddInManager get() {
		return INSTANCE;
	}
	
	public void start() {
		if (UtilImpl.ADDINS_DIRECTORY == null) {
			UtilImpl.LOGGER.info("Add-Ins directory = " + UtilImpl.CONTENT_DIRECTORY + "addins/");
			plugInManager = new DefaultPluginManager(Paths.get(UtilImpl.CONTENT_DIRECTORY, "addins"));
		}
		else {
			UtilImpl.LOGGER.info("Add-Ins directory = " + UtilImpl.ADDINS_DIRECTORY);
			plugInManager = new DefaultPluginManager(Paths.get(UtilImpl.ADDINS_DIRECTORY));
		}
		plugInManager.loadPlugins();
		plugInManager.startPlugins();
		
		for (PluginWrapper plugin : plugInManager.getStartedPlugins()) {
			UtilImpl.LOGGER.info("Add-in " + plugin.getPluginId() + " : " + plugin.getDescriptor() + " has started.");
			for (Class<?> extension : plugInManager.getExtensionClasses(plugin.getPluginId())) {
				UtilImpl.LOGGER.info("    Extension " + extension + " has been registered.");
			}
		}

		for (PluginWrapper plugin : plugInManager.getUnresolvedPlugins()) {
			UtilImpl.LOGGER.warning("Add-in " + plugin.getPluginId() + " : " + plugin.getDescriptor() + " is unresolved.");
		}
	}
	
	public void stop() {
		if (plugInManager != null) {
			plugInManager.stopPlugins();
			plugInManager = null;
		}
	}
	
	@Override
	public <T extends Object> T getExtension(Class<T> type) {
		if (plugInManager != null) {
			List<T> extensions = plugInManager.getExtensions(type);
			if ((extensions != null) && (! extensions.isEmpty())) {
				return extensions.get(0);
			}
		}
		
		return null;
	}
}
