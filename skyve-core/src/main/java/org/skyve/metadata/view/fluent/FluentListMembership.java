package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;

/**
 * Builds {@link ListMembership} input widget metadata.
 */
public class FluentListMembership extends FluentInputWidget<FluentListMembership> {
	private ListMembership list = null;

	/**
	 * Creates a builder backed by a new {@link ListMembership}.
	 */
	public FluentListMembership() {
		list = new ListMembership();
	}

	/**
	 * Creates a builder backed by the supplied {@link ListMembership}.
	 *
	 * @param list
	 *            the metadata instance to mutate
	 */
	public FluentListMembership(ListMembership list) {
		this.list = list;
	}

	/**
	 * Copies membership state and changed-event actions from runtime metadata.
	 *
	 * @param list
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentListMembership from(@SuppressWarnings("hiding") ListMembership list) {

		list.getChangedActions().forEach(c -> addChangedAction(FluentEventAction.from(c)));

		super.from(list);
		return this;
	}

	/**
	 * Appends a changed-event action.
	 *
	 * @param action
	 *            the action metadata to append
	 * @return this builder
	 */
	public FluentListMembership addChangedAction(FluentEventAction action) {
		list.getChangedActions().add(action.get());
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped list-membership metadata
	 */
	@Override
	public ListMembership get() {
		return list;
	}
}
