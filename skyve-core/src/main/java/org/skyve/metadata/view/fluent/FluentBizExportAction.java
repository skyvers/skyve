package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.BizExportAction;

public class FluentBizExportAction extends FluentClassAction<FluentBizExportAction> {
	private BizExportAction action = null;
	
	public FluentBizExportAction() {
		action = new BizExportAction();
	}
	
	public FluentBizExportAction(BizExportAction action) {
		this.action = action;
	}
	
	public FluentBizExportAction from(@SuppressWarnings("hiding") BizExportAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public BizExportAction get() {
		return action;
	}
}
