package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.SetDisabledEventAction;

/**
 * Builds {@link SetDisabledEventAction} metadata using a fluent API.
 */
public class FluentSetDisabledEventAction extends FluentEventAction {
	private SetDisabledEventAction action = null;

	/**
	 * Creates a fluent builder backed by a new {@link SetDisabledEventAction} instance.
	 */
	public FluentSetDisabledEventAction() {
		action = new SetDisabledEventAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentSetDisabledEventAction(SetDisabledEventAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentSetDisabledEventAction from(@SuppressWarnings("hiding") SetDisabledEventAction action) {
		binding(action.getBinding());
		disabledConditionName(action.getDisabledConditionName());

		return this;
	}

	/**
	 * Sets the target binding updated by this event action.
	 *
	 * @param binding
	 *            the target binding expression
	 * @return this builder
	 */
	public FluentSetDisabledEventAction binding(String binding) {
		action.setBinding(binding);
		return this;
	}

	/**
	 * Sets the condition used to compute the disabled state.
	 *
	 * @param disabledConditionName
	 *            the condition name
	 * @return this builder
	 */
	public FluentSetDisabledEventAction disabledConditionName(String disabledConditionName) {
		action.setDisabledConditionName(disabledConditionName);
		return this;
	}

	/**
	 * Returns the wrapped event action instance.
	 */
	@Override
	public SetDisabledEventAction get() {
		return action;
	}
}
