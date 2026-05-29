package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.PositionableAction;
import org.skyve.metadata.view.Action.ActionShow;

/**
 * Adds placement and visibility controls to fluent action builders.
 *
 * @param <T>
 *            the concrete fluent action type
 */
public abstract class FluentPositionableAction<T extends FluentPositionableAction<T>> extends FluentAction<T> {
	/**
	 * Creates a positionable action fluent base.
	 */
	protected FluentPositionableAction() {
		// nothing to see here
	}

	/**
	 * Copies position-related metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	protected T from(PositionableAction action) {
		super.fromBase(action);
		inActionPanel(! Boolean.FALSE.equals(action.getInActionPanel()));
		show(action.getShow());
		return (T) this;
	}
	
	/**
	 * Sets whether the action should appear in the action panel.
	 *
	 * @param inActionPanel
	 *            whether to render in the action panel
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T inActionPanel(boolean inActionPanel) {
		get().setInActionPanel(inActionPanel ? Boolean.TRUE : Boolean.FALSE);
		return (T) this;
	}
	
	/**
	 * Sets where this action is shown by the UI renderer.
	 *
	 * @param show
	 *            the display placement
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T show(ActionShow show) {
		get().setShow(show);
		return (T) this;
	}

	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public abstract PositionableAction get();
}
