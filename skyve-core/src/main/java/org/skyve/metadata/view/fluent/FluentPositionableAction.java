package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.PositionableAction;
import org.skyve.metadata.view.Action.ActionShow;

abstract class FluentPositionableAction<T extends FluentPositionableAction<T>> extends FluentAction<T> {
	protected FluentPositionableAction() {
		// nothing to see here
	}

	@SuppressWarnings("unchecked")
	protected T from(PositionableAction action) {
		super.fromBase(action);
		inActionPanel(! Boolean.FALSE.equals(action.getInActionPanel()));
		show(action.getShow());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T inActionPanel(boolean inActionPanel) {
		get().setInActionPanel(inActionPanel ? Boolean.TRUE : Boolean.FALSE);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T show(ActionShow show) {
		get().setShow(show);
		return (T) this;
	}

	@Override
	public abstract PositionableAction get();
}
