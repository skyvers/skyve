package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.BizImportAction;

public class FluentBizImportAction extends FluentClassAction<FluentBizImportAction> {
	private BizImportAction action = null;
	
	public FluentBizImportAction() {
		action = new BizImportAction();
	}
	
	public FluentBizImportAction(BizImportAction action) {
		this.action = action;
	}
	
	public FluentBizImportAction from(@SuppressWarnings("hiding") BizImportAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public BizImportAction get() {
		return action;
	}
}
