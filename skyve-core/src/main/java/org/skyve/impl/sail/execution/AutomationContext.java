package org.skyve.impl.sail.execution;

import org.skyve.impl.web.UserAgentType;
import org.skyve.metadata.view.View.ViewType;

public class AutomationContext {
	private String moduleName;
	private String documentName;
	private ViewType viewType;
	private String uxui;
	private UserAgentType userAgentType;

	public AutomationContext() {
		// nothing to see here
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
/*	
	public Document getDrivingDocument() {
		Customer c = CORE.getUser().getCustomer();
		return c.getModule(moduleName).getDocument(c, documentName);
	}
*/
}
