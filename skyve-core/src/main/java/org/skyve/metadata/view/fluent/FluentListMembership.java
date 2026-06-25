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
		candidatesHeading(list.getCandidatesHeading());
		escapeCandidatesHeading(list.getEscapeCandidatesHeading());
		membersHeading(list.getMembersHeading());
		escapeMembersHeading(list.getEscapeMembersHeading());

		list.getChangedActions().forEach(c -> addChangedAction(FluentEventAction.from(c)));

		super.from(list);
		return this;
	}

	/**
	 * Sets the candidate-list heading.
	 *
	 * @param candidatesHeading heading text
	 * @return this builder
	 */
	public FluentListMembership candidatesHeading(String candidatesHeading) {
		list.setCandidatesHeading(candidatesHeading);
		return this;
	}

	/**
	 * Sets whether the candidate-list heading should be escaped before rendering.
	 *
	 * @param escapeCandidatesHeading {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentListMembership escapeCandidatesHeading(boolean escapeCandidatesHeading) {
		return escapeCandidatesHeading(escapeCandidatesHeading ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the candidate-list heading should be escaped before rendering.
	 *
	 * @param escapeCandidatesHeading {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentListMembership escapeCandidatesHeading(Boolean escapeCandidatesHeading) {
		list.setEscapeCandidatesHeading(escapeCandidatesHeading);
		return this;
	}

	/**
	 * Sets the members-list heading.
	 *
	 * @param membersHeading heading text
	 * @return this builder
	 */
	public FluentListMembership membersHeading(String membersHeading) {
		list.setMembersHeading(membersHeading);
		return this;
	}

	/**
	 * Sets whether the members-list heading should be escaped before rendering.
	 *
	 * @param escapeMembersHeading {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentListMembership escapeMembersHeading(boolean escapeMembersHeading) {
		return escapeMembersHeading(escapeMembersHeading ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the members-list heading should be escaped before rendering.
	 *
	 * @param escapeMembersHeading {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentListMembership escapeMembersHeading(Boolean escapeMembersHeading) {
		list.setEscapeMembersHeading(escapeMembersHeading);
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
