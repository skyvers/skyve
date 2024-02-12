package modules.admin.Configuration;

import org.skyve.util.test.SkyveFactory;

import modules.admin.Configuration.actions.SendMail;

@SkyveFactory(testDomain = false, excludedActions = { SendMail.class })
public class ConfigurationFactory {
	// nothing to see here
}
