package org.skyve.impl.util;

import java.io.Serializable;
import java.util.Locale;

/**
 * Immutable customer-level two-factor-authentication configuration payload.
 */
public class TwoFactorAuthCustomerConfiguration implements Serializable {
	private static final long serialVersionUID = -4834211404762691050L;

	/**
	 * Supported two-factor modes.
	 */
	public enum TfaType {
		/** Two-factor is disabled. */
		OFF,
		/** Two-factor uses email delivery. */
		EMAIL,
		/** Stored code was unknown/invalid. */
		UNSUPPORTED;

		/**
		 * Parses a persisted TFA code into an enum value.
		 *
		 * @param code Persisted mode code
		 * @return Matching type, or {@link #UNSUPPORTED}
		 */
		static TfaType fromCode(String code) {
			if (code == null) {
				return UNSUPPORTED;
			}
			try {
				return TfaType.valueOf(code.toUpperCase(Locale.ROOT));
			}
			catch (@SuppressWarnings("unused") IllegalArgumentException e) {
				return UNSUPPORTED;
			}
		}
	}

	private final String tfaType;
	private final TfaType parsedTfaType;
	private final int tfaTimeOutSeconds;
	private final String twoFactorEmailSubject;
	private final String twoFactorEmailBody;
	
	/**
	 * Creates a customer-level two-factor configuration.
	 *
	 * @param tfaType Persisted TFA mode code
	 * @param tfaTimeOutSeconds Challenge timeout in seconds
	 * @param twoFactorEmailSubject Email subject template
	 * @param twoFactorEmailBody Email body template
	 */
	public TwoFactorAuthCustomerConfiguration(String tfaType,
											int tfaTimeOutSeconds,
											String twoFactorEmailSubject,
											String twoFactorEmailBody) {
		this.tfaType = tfaType;
		this.parsedTfaType = TfaType.fromCode(tfaType);
		this.tfaTimeOutSeconds = tfaTimeOutSeconds;
		this.twoFactorEmailSubject = twoFactorEmailSubject;
		this.twoFactorEmailBody = twoFactorEmailBody;
	}
	
	/**
	 * Returns the raw persisted TFA mode code.
	 */
	public String getTfaType() {
		return tfaType;
	}

	/**
	 * Returns the parsed enum representation of {@link #getTfaType()}.
	 */
	public TfaType getParsedTfaType() {
		return parsedTfaType;
	}

	/**
	 * Returns the challenge timeout in seconds.
	 */
	public int getTfaTimeOutSeconds() {
		return tfaTimeOutSeconds;
	}

	/**
	 * Returns the email subject template for two-factor challenges.
	 */
	public String getTwoFactorEmailSubject() {
		return twoFactorEmailSubject;
	}

	/**
	 * Returns the email body template for two-factor challenges.
	 */
	public String getTwoFactorEmailBody() {
		return twoFactorEmailBody;
	}

	/**
	 * Indicates whether TFA is explicitly disabled.
	 */
	public boolean isTfaOff() {
		return parsedTfaType == TfaType.OFF;
	}

	/**
	 * Indicates whether email-based TFA is enabled.
	 */
	public boolean isTfaEmail() {
		return parsedTfaType == TfaType.EMAIL;
	}
}
