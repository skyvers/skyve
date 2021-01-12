package org.skyve.addin;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.pf4j.ClassLoadingStrategy;
import org.pf4j.CompoundPluginLoader;
import org.pf4j.DefaultPluginLoader;
import org.pf4j.DefaultPluginManager;
import org.pf4j.DevelopmentPluginLoader;
import org.pf4j.JarPluginLoader;
import org.pf4j.PluginClassLoader;
import org.pf4j.PluginDescriptor;
import org.pf4j.PluginLoader;
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
		plugInManager = new DefaultPluginManager(Paths.get(UtilImpl.CONTENT_DIRECTORY, "addins")) {
//			@Override
//			public RuntimeMode getRuntimeMode() {
//				return RuntimeMode.DEVELOPMENT;
//			}

			@Override
			public ClassLoader getPluginClassLoader(String pluginId) {
				// TODO Auto-generated method stub
				return super.getPluginClassLoader(pluginId);
			}
			
			@Override
			protected PluginLoader createPluginLoader() {
		        return new CompoundPluginLoader()
		                .add(new DevelopmentPluginLoader(this) {
		                	@Override
		                	protected PluginClassLoader createPluginClassLoader(Path pluginPath, PluginDescriptor pluginDescriptor) {
		                        return new PluginClassLoader(pluginManager, pluginDescriptor, getClass().getClassLoader(), ClassLoadingStrategy.ADP);
		                	}
		                }, this::isDevelopment)
		                .add(new JarPluginLoader(this) {
		                    @Override
		                    public ClassLoader loadPlugin(Path pluginPath, PluginDescriptor pluginDescriptor) {
		                        PluginClassLoader pluginClassLoader = new PluginClassLoader(pluginManager, pluginDescriptor, getClass().getClassLoader(), ClassLoadingStrategy.ADP);
		                        pluginClassLoader.addFile(pluginPath.toFile());

		                        return pluginClassLoader;
		                    }
		                }, this::isNotDevelopment)
		                .add(new DefaultPluginLoader(this) {
		                	@Override
		                	protected PluginClassLoader createPluginClassLoader(Path pluginPath, PluginDescriptor pluginDescriptor) {
		                        return new PluginClassLoader(pluginManager, pluginDescriptor, getClass().getClassLoader(), ClassLoadingStrategy.ADP);
		                	}
		                }, this::isNotDevelopment);
			}
		};
		plugInManager.loadPlugins();
		plugInManager.startPlugins();
		
		for (PluginWrapper plugin : plugInManager.getStartedPlugins()) {
			System.out.println("Plugin " + plugin.getPluginId() + " : " + plugin.getDescriptor() + " has started.");
			for (Class<?> extension : plugInManager.getExtensionClasses(plugin.getPluginId())) {
				System.out.println("    Extension " + extension + " has been registered.");
			}
		}

		for (PluginWrapper plugin : plugInManager.getUnresolvedPlugins()) {
			System.out.println("Plugin " + plugin.getPluginId() + " : " + plugin.getDescriptor() + " is unresolved.");
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
