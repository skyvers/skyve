package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;

/**
 * Builds {@link SetInvisibleEventAction} metadata using a fluent API.
 */
public class FluentSetInvisibleEventAction extends FluentEventAction {
	private SetInvisibleEventAction action = null;

	/**
	 * Creates a fluent builder backed by a new {@link SetInvisibleEventAction} instance.
	 */
	public FluentSetInvisibleEventAction() {
		action = new SetInvisibleEventAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentSetInvisibleEventAction(SetInvisibleEventAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentSetInvisibleEventAction from(@SuppressWarnings("hiding") SetInvisibleEventAction action) {
		binding(action.getBinding());
		invisibleConditionName(action.getInvisibleConditionName());
		return this;
	}

	/**
	 * Sets the target binding updated by this event action.
	 *
	 * @param binding
	 *            the target binding expression
	 * @return this builder
	 */
	public FluentSetInvisibleEventAction binding(String binding) {
		action.setBinding(binding);
		return this;
	}

	/**
	 * Sets the condition used to compute the invisible state.
	 *
	 * @param invisibleConditionName
	 *            the condition name
	 * @return this builder
	 */
	public FluentSetInvisibleEventAction invisibleConditionName(String invisibleConditionName) {
		action.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Returns the wrapped event action instance.
	 */
	@Override
	public SetInvisibleEventAction get() {
		return action;
	}
}
