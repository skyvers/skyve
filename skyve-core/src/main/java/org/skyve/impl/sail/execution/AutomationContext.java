package org.skyve.impl.sail.execution;

import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.UserAgentType;

/**
 * Represents the context for SAIL automation.
 * 
 * @author mike
 */
public class AutomationContext {

	private String moduleName;
	private String documentName;
	private ViewType viewType;
	private String uxui;
	private UserAgentType userAgentType;

	public AutomationContext() {
		// Nothing to see here
	}

	public AutomationContext(AutomationContext context) {
		moduleName = context.moduleName;
		documentName = context.documentName;
		viewType = context.viewType;
		uxui = context.uxui;
		userAgentType = context.userAgentType;
	}
	
	public String getModuleName() {
		return moduleName;
	}

	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	public String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	public ViewType getViewType() {
		return viewType;
	}
	
	public void setViewType(ViewType viewType) {
		this.viewType = viewType;
	}
	
	public String getUxui() {
		return uxui;
	}
	
	public void setUxui(String uxui) {
		this.uxui = uxui;
	}
	
	public UserAgentType getUserAgentType() {
		return userAgentType;
	}
	
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}
}
