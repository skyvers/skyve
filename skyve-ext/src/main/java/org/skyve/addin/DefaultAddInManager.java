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
		plugInManager = new DefaultPluginManager(Paths.get(UtilImpl.CONTENT_DIRECTORY, "addins"));
		plugInManager.loadPlugins();
		plugInManager.startPlugins();
		
		for (PluginWrapper plugin : plugInManager.getStartedPlugins()) {
			System.out.println("Plugin " + plugin.getPluginId() + " : " + plugin.getDescriptor() + " has started.");
			for (Class<?> extension : plugInManager.getExtensionClasses(plugin.getPluginId())) {
				System.out.println("    Exstenion " + extension + " has been registered.");
			}
		}

		for (PluginWrapper plugin : plugInManager.getUnresolvedPlugins()) {
			System.out.println("Plugin " + plugin.getPluginId() + " : " + plugin.getDescriptor() + " is unresolved.");
		}
	}
	
	public void stop() {
		plugInManager.stopPlugins();
	}
	
	@Override
	public <T extends Object> T getAddIn(Class<T> type) {
		List<T> extensions = plugInManager.getExtensions(type);
		if ((extensions != null) && (! extensions.isEmpty())) {
			return extensions.get(0);
		}
		
		return null;
	}
}
