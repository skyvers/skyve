package org.skyve.impl.sms;

import org.skyve.util.SMSService;

/**
 * No SMS services.
 * This is set when there is no configured SMS service in the JSON.
 */
public class NoOpSMSService implements SMSService {
	@Override
	public boolean text(String toPhoneNumber, String message) {
		return false;
	}

	@Override
	public boolean text(String toPhoneNumber, String fromPhoneNumber, String message) {
		return false;
	}
}
