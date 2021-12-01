package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.UploadAction;

public class FluentUploadAction extends FluentClassAction<FluentUploadAction> {
	private UploadAction action = null;
	
	public FluentUploadAction() {
		action = new UploadAction();
	}
	
	public FluentUploadAction(UploadAction action) {
		this.action = action;
	}
	
	public FluentUploadAction from(@SuppressWarnings("hiding") UploadAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public UploadAction get() {
		return action;
	}
}
