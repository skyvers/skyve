package modules.admin.Startup;

import org.skyve.util.test.SkyveFactory;

import modules.admin.Startup.actions.SaveChanges;

@SkyveFactory(excludedActions = { SaveChanges.class })
public class StartupFactory {
	// builder defaults
}
