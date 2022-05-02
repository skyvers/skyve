package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.PrintAction;

public class FluentPrintAction extends FluentValidatableAction<FluentPrintAction> {
	private PrintAction action = null;
	
	public FluentPrintAction() {
		action = new PrintAction();
	}
	
	public FluentPrintAction(PrintAction action) {
		this.action = action;
	}
	
	public FluentPrintAction from(@SuppressWarnings("hiding") PrintAction action) {
		super.fromBase(action);
		return this;
	}
	
	@Override
	public PrintAction get() {
		return action;
	}
}
