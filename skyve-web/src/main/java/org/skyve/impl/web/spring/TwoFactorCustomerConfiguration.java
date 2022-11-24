package org.skyve.impl.web.spring;

public class TwoFactorCustomerConfiguration {
	private String tfaType;
	private int tfaTimeOutSeconds;
	private String twoFactorEmailSubject;
	private String twoFactorEmailBody;
	
	public TwoFactorCustomerConfiguration(String tfaType,
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
}
