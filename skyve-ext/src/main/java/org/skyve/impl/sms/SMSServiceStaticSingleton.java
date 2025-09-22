package org.skyve.impl.sms;

import org.skyve.util.SMSService;

/**
 * A singleton for the SMS service to use.
 * NB This doesn't need to be thread safe as it is set at startup only.
 */
public class SMSServiceStaticSingleton {
	private static SMSService instance;
	
	private SMSServiceStaticSingleton() {
		// nothing to see here
	}
	
	public static SMSService get() {
		return instance;
	}
	
	public static void set(SMSService instance) {
		SMSServiceStaticSingleton.instance = instance;
	}

	public static void setDefault() {
		instance = new NoOpSMSService();
	}
}
