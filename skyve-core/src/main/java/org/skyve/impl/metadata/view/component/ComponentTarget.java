package org.skyve.impl.metadata.view.component;

import java.io.Serializable;

/**
 * Encapsulates information to be able to find the view a view component points to.
 * This also holds the lastModifiedMillis for dev mode.
 */
public class ComponentTarget implements Serializable {
	private static final long serialVersionUID = -5638663548937378098L;

	private String overriddenCustomerName;
	private String moduleName;
	private String documentName;
	private String overriddenUxUi;
	private String viewName;
	private long lastModifiedMillis;
	
	ComponentTarget(String overriddenCustomerName,
						String moduleName,
						String documentName,
						String overriddenUxUi,
						String viewName,
						long lastModifiedMillis) {
		this.overriddenCustomerName = overriddenCustomerName;
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.overriddenUxUi = overriddenUxUi;
		this.viewName = viewName;
		this.lastModifiedMillis = lastModifiedMillis;
	}

	public String getOverriddenCustomerName() {
		return overriddenCustomerName;
	}

	public String getModuleName() {
		return moduleName;
	}

	public String getDocumentName() {
		return documentName;
	}

	public String getOverriddenUxUi() {
		return overriddenUxUi;
	}

	public String getViewName() {
		return viewName;
	}

	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}
}
