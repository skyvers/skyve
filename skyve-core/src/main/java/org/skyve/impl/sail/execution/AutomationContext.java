package org.skyve.impl.sail.execution;

import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract base context for SAIL test automation, parameterised by list and edit context types, dependent on the UI.
 *
 * @param <GLC> the type of the list generation context
 * @param <GEC> the type of the edit generation context
 * @author simeonsolomou
 */
public abstract class AutomationContext<GLC extends GenerateListContext, GEC extends GenerateEditContext> {

	private String moduleName;
	private String documentName;
	private ViewType viewType;
	private String uxui;
	private UserAgentType userAgentType;

	public AutomationContext() {
		// Nothing to see here
	}

	public AutomationContext(@Nonnull AutomationContext<?, ?> context) {
		moduleName = context.moduleName;
		documentName = context.documentName;
		viewType = context.viewType;
		uxui = context.uxui;
		userAgentType = context.userAgentType;
	}
	
	public @Nullable String getModuleName() {
		return moduleName;
	}

	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = moduleName;
	}

	public @Nullable String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(@Nullable String documentName) {
		this.documentName = documentName;
	}

	public @Nullable ViewType getViewType() {
		return viewType;
	}
	
	public void setViewType(@Nullable ViewType viewType) {
		this.viewType = viewType;
	}
	
	public @Nullable String getUxui() {
		return uxui;
	}
	
	public void setUxui(@Nullable String uxui) {
		this.uxui = uxui;
	}
	
	public @Nullable UserAgentType getUserAgentType() {
		return userAgentType;
	}
	
	public void setUserAgentType(@Nullable UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}

	/**
	 * Generates automation context for test execution in a list view.
	 *
	 * @param listContext the list view context used for automation
	 */
	public abstract void generate(@Nonnull GLC listContext);

	/**
	 * Generates automation context for test execution in an edit view.
	 *
	 * @param editContext the edit view context used for automation
	 */
	public abstract void generate(@Nonnull GEC editContext);
}
