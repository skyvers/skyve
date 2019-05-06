package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.customer.LoginResources;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "LoginResources")
public class LoginResourcesMetaData implements LoginResources {
	private static final long serialVersionUID = -6851698152410454808L;

	private String loginPageURL;
	private String loggedOutPageURL;
	private String smartClientJavascriptURL;

	@Override
	public String getLoginPageURL() {
		return loginPageURL;
	}
	
	@XmlAttribute(name = "loginPage")
	public void setLoginPageURL(String loginPageURL) {
		this.loginPageURL = UtilImpl.processStringValue(loginPageURL);
	}
	
	@Override
	public String getLoggedOutPageURL() {
		return loggedOutPageURL;
	}
	
	@XmlAttribute(name = "loggedOutPage")
	public void setLoggedOutPageURL(String loggedOutPageURL) {
		this.loggedOutPageURL = UtilImpl.processStringValue(loggedOutPageURL);
	}
	
	@Override
	public String getSmartClientJavascriptURL() {
		return smartClientJavascriptURL;
	}
	
	@XmlAttribute(name = "smartClientJavascript")
	public void setSmartClientjavascriptURL(String smartClientJavascriptURL) {
		this.smartClientJavascriptURL = UtilImpl.processStringValue(smartClientJavascriptURL);
	}
}
