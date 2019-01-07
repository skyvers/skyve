package modules.admin.Configuration;

import org.skyve.impl.util.UtilImpl;

import modules.admin.domain.Configuration;

public class ConfigurationExtension extends Configuration {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5669557826609528645L;

	/**
	 * Check whether a valid SMTP host has been configured
	 * 
	 * @return
	 */
	public static boolean validSMTPHost() {
		return !"localhost".equals(UtilImpl.SMTP);
	}
	
	public static String  defaultSMTPSender() {
		return UtilImpl.SMTP_SENDER;
	}

}
