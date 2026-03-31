package org.skyve.impl.mail;

import java.util.Objects;

import org.skyve.util.MailService;

import jakarta.annotation.Nonnull;

/**
 * A singleton holder for the configured raw mail service and its decorated runtime facade.
 */
public class MailServiceStaticSingleton {
	private static volatile MailService configuredInstance;
	private static volatile MailService effectiveInstance;

	static {
		setDefault();
	}

	private MailServiceStaticSingleton() {
		// nothing to see here
	}

	/**
	 * Return the configured raw mail service implementation.
	 */
	public static @Nonnull MailService get() {
		MailService result = configuredInstance;
		if (result == null) {
			throw new IllegalStateException("MailService has not been initialised.");
		}
		return result;
	}

	/**
	 * Return the configured mail service decorated with framework-wide pre-processing and logging.
	 */
	public static @Nonnull MailService getEffective() {
		MailService result = effectiveInstance;
		if (result == null) {
			throw new IllegalStateException("Effective MailService has not been initialised.");
		}
		return result;
	}

	public static void set(@Nonnull MailService instance) {
		MailService configured = Objects.requireNonNull(instance, "instance");
		configuredInstance = configured;
		effectiveInstance = new PreProcessingMailService(new LoggingMailService(configured));
	}

	public static void setDefault() {
		set(new SMTPMailService());
	}
}
