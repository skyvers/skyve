package org.skyve.impl.addin;

import java.nio.file.Paths;
import java.util.List;

import org.pf4j.DefaultPluginManager;
import org.pf4j.PluginManager;
import org.pf4j.PluginWrapper;
import org.skyve.addin.AddInManager;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PF4JAddInManager implements AddInManager {
	private static final PF4JAddInManager INSTANCE = new PF4JAddInManager();
	private static final Logger LOGGER = LoggerFactory.getLogger(PF4JAddInManager.class);

	private PluginManager plugInManager;

	private PF4JAddInManager() {
		// nothing to see here
	}

	public static PF4JAddInManager get() {
		return INSTANCE;
	}
	
	@Override
	public void startup() {
		String addinsDirectory = Util.getAddinsDirectory();
		LOGGER.info("Add-Ins directory = " + addinsDirectory);
		plugInManager = new DefaultPluginManager(Paths.get(Util.getAddinsDirectory()));
		plugInManager.loadPlugins();
		plugInManager.startPlugins();
		
		for (PluginWrapper plugin : plugInManager.getStartedPlugins()) {
			LOGGER.info("Add-in " + plugin.getPluginId() + " : " + plugin.getDescriptor() + " has started.");
			for (Class<?> extension : plugInManager.getExtensionClasses(plugin.getPluginId())) {
				LOGGER.info("    Extension " + extension + " has been registered.");
			}
		}

		for (PluginWrapper plugin : plugInManager.getUnresolvedPlugins()) {
			LOGGER.warn("Add-in " + plugin.getPluginId() + " : " + plugin.getDescriptor() + " is unresolved.");
		}
	}
	
	@Override
	public void shutdown() {
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
