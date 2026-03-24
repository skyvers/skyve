package org.skyve.impl.mail;

import org.skyve.util.MailService;

/**
 * A singleton for the mail service to use.
 * NB This doesn't need to be thread safe as it is set at startup only.
 */
public class MailServiceStaticSingleton {
	private static MailService instance;
	
	private MailServiceStaticSingleton() {
		// nothing to see here
	}
	
	public static MailService get() {
		if (instance == null) {
			setDefault();
		}
		return instance;
	}
	
	public static void set(MailService instance) {
		MailServiceStaticSingleton.instance = instance;
	}

	public static void setDefault() {
		instance = new SMTPMailService();
	}
}
