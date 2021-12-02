package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.LoginResourcesMetaData;
import org.skyve.metadata.customer.LoginResources;

public class FluentLoginResources {
	private LoginResourcesMetaData resources = null;
	
	public FluentLoginResources() {
		resources = new LoginResourcesMetaData();
	}

	public FluentLoginResources(LoginResourcesMetaData resources) {
		this.resources = resources;
	}
	
	public FluentLoginResources from(@SuppressWarnings("hiding") LoginResources resources) {
		loginPageURL(resources.getLoginPageURL());
		loggedOutPageURL(resources.getLoggedOutPageURL());
		smartClientJavascriptURL(resources.getSmartClientJavascriptURL());
		return this;
	}
	
	public FluentLoginResources loginPageURL(String url) {
		resources.setLoginPageURL(url);
		return this;
	}

	public FluentLoginResources loggedOutPageURL(String url) {
		resources.setLoggedOutPageURL(url);
		return this;
	}

	public FluentLoginResources smartClientJavascriptURL(String url) {
		resources.setSmartClientJavascriptURL(url);
		return this;
	}

	public LoginResourcesMetaData get() {
		return resources;
	}
}
