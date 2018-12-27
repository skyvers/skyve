package modules.admin.Configuration;

import org.skyve.impl.util.UtilImpl;

import modules.admin.domain.Configuration;

public class ConfigurationExtension extends Configuration {

	/**
	 * Check whether a valid SMTP host has been configured
	 * 
	 * @return
	 */
	public static boolean validSMTPHost() {
		return !"localhost".equals(UtilImpl.SMTP);
	}

}
