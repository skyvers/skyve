package org.skyve.impl.sms;

import org.skyve.util.SMSService;

import jakarta.annotation.Nonnull;

/**
 * No SMS services.
 * This is set when there is no configured SMS service in the JSON.
 */
public class NoOpSMSService implements SMSService {
	@Override
	public boolean text(@Nonnull String toPhoneNumber, @Nonnull String message) {
		return false;
	}

	@Override
	public boolean text(@Nonnull String toPhoneNumber, @Nonnull String fromPhoneNumber, @Nonnull String message) {
		return false;
	}
}
