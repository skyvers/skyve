package org.skyve.impl.util;

import java.io.Serializable;
import java.util.Locale;

public class TwoFactorAuthCustomerConfiguration implements Serializable {
	private static final long serialVersionUID = -4834211404762691050L;

	public enum TfaType {
		OFF,
		EMAIL,
		UNSUPPORTED;

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
	
	public String getTfaType() {
		return tfaType;
	}

	public TfaType getParsedTfaType() {
		return parsedTfaType;
	}

	public int getTfaTimeOutSeconds() {
		return tfaTimeOutSeconds;
	}

	public String getTwoFactorEmailSubject() {
		return twoFactorEmailSubject;
	}

	public String getTwoFactorEmailBody() {
		return twoFactorEmailBody;
	}

	public boolean isTfaOff() {
		return parsedTfaType == TfaType.OFF;
	}

	public boolean isTfaEmail() {
		return parsedTfaType == TfaType.EMAIL;
	}
}
