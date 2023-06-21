package org.skyve.impl.util;

import java.io.Serializable;

public class TwoFactorAuthCustomerConfiguration implements Serializable {
	private static final long serialVersionUID = -4834211404762691050L;

	private String tfaType;
	private int tfaTimeOutSeconds;
	private String twoFactorEmailSubject;
	private String twoFactorEmailBody;
	
	public TwoFactorAuthCustomerConfiguration(String tfaType,
											int tfaTimeOutSeconds,
											String twoFactorEmailSubject,
											String twoFactorEmailBody) {
		this.tfaType = tfaType;
		this.tfaTimeOutSeconds = tfaTimeOutSeconds;
		this.twoFactorEmailSubject = twoFactorEmailSubject;
		this.twoFactorEmailBody = twoFactorEmailBody;
		
	}
	
	public String getTfaType() {
		return tfaType;
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
	public boolean isTfaEmail() {
		return "EMAIL".equals(tfaType);
	}
}
