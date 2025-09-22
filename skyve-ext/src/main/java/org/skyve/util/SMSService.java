package org.skyve.util;

import jakarta.annotation.Nonnull;

/**
 * An SMS service interface for Skyve.
 */
public interface SMSService {
	/**
	 * Send the <code>message</code> as a text message from the default phone number to the <code>toPhoneNumber</code>.
	 * 
	 * @param toPhoneNumber	The phone number to send to.
	 * @param message	The message to send.
	 * @return	<code>true</code> if successfully sent, otherwise <code>false</code>.
	 */
	boolean text(@Nonnull String toPhoneNumber, @Nonnull String message);

	/**
	 * Send the <code>message</code> as a text message from the <code>fromPhoneNumber</code> to the <code>toPhoneNumber</code>.
	 * 
	 * @param fromPhoneNumber	The phone number to send from.
	 * @param toPhoneNumber	The phone number to send to.
	 * @param message	The message to send.
	 * @return	<code>true</code> if successfully sent, otherwise <code>false</code>.
	 */
	boolean text(@Nonnull String toPhoneNumber, @Nonnull String fromPhoneNumber, @Nonnull String message);
}
