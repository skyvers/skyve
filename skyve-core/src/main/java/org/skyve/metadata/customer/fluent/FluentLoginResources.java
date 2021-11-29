package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.LoginResourcesMetaData;
import org.skyve.metadata.customer.LoginResources;

public class FluentLoginResources {
	private LoginResourcesMetaData resources = new LoginResourcesMetaData();
	
	public FluentLoginResources() {
		// nothing to see here
	}
	
	public FluentLoginResources(LoginResources resources) {
		loginPageURL(resources.getLoginPageURL());
		loggedOutPageURL(resources.getLoggedOutPageURL());
		smartClientjavascriptURL(resources.getSmartClientJavascriptURL());
	}
	
	public FluentLoginResources loginPageURL(String url) {
		resources.setLoginPageURL(url);
		return this;
	}

	public FluentLoginResources loggedOutPageURL(String url) {
		resources.setLoggedOutPageURL(url);
		return this;
	}

	public FluentLoginResources smartClientjavascriptURL(String url) {
		resources.setSmartClientjavascriptURL(url);
		return this;
	}

	public LoginResourcesMetaData get() {
		return resources;
	}
}
