package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.DownloadAction;

public class FluentDownloadAction extends FluentClassAction<FluentDownloadAction> {
	private DownloadAction action = null;
	
	public FluentDownloadAction() {
		action = new DownloadAction();
	}
	
	public FluentDownloadAction(DownloadAction action) {
		this.action = action;
	}
	
	public FluentDownloadAction from(@SuppressWarnings("hiding") DownloadAction action) {
		super.fromBase(action);
		return this;
	}
	
	@Override
	public DownloadAction get() {
		return action;
	}
}
