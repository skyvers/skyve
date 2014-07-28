package org.skyve.wildcat.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.CUSTOMER_NAMESPACE)
public class LoginResources implements org.skyve.metadata.customer.LoginResources {
	private String loginPageRelativeFileName;
	private String loginErrorPageRelativeFileName;
	private String loggedOutPageRelativeFileName;
	private String smartClientJavascriptRelativeFileName;

	@Override
	public String getLoginPageRelativeFileName() {
		return loginPageRelativeFileName;
	}
	
	@XmlAttribute(name = "loginPage")
	public void setLoginPageRelativeFileName(String loginPageRelativeFileName) {
		this.loginPageRelativeFileName = UtilImpl.processStringValue(loginPageRelativeFileName);
	}
	
	@Override
	public String getLoginErrorPageRelativeFileName() {
		return loginErrorPageRelativeFileName;
	}
	
	@XmlAttribute(name = "loginErrorPage")
	public void setLoginErrorPageRelativeFileName(String loginErrorPageRelativeFileName) {
		this.loginErrorPageRelativeFileName = UtilImpl.processStringValue(loginErrorPageRelativeFileName);
	}
	
	@Override
	public String getLoggedOutPageRelativeFileName() {
		return loggedOutPageRelativeFileName;
	}
	
	@XmlAttribute(name = "loggedOutPage")
	public void setLoggedOutPageRelativeFileName(String loggedOutPageRelativeFileName) {
		this.loggedOutPageRelativeFileName = UtilImpl.processStringValue(loggedOutPageRelativeFileName);
	}
	
	@Override
	public String getSmartClientJavascriptRelativeFileName() {
		return smartClientJavascriptRelativeFileName;
	}
	
	@XmlAttribute(name = "smartClientJavascript")
	public void setSmartClientjavascriptRelativeFileName(String smartClientJavascriptRelativeFileName) {
		this.smartClientJavascriptRelativeFileName = UtilImpl.processStringValue(smartClientJavascriptRelativeFileName);
	}
}
