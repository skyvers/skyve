package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
		BindUtil.copyAuthoredCondition(action.getXmlDisabledCondition(), action.getXmlEnabledCondition(), this::disabledCondition, this::enabledCondition);
		action.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

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
	 * Sets the {@code disabled} condition as authored; the opposite sense of {@link #enabledCondition(String)}.
	 *
	 * @param disabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentSetDisabledEventAction disabledCondition(@Nullable String disabledCondition) {
		action.setXmlDisabledCondition(disabledCondition);
		return this;
	}

	/**
	 * Sets the {@code enabled} condition as authored; the opposite sense of {@link #disabledCondition(String)}.
	 *
	 * @param enabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentSetDisabledEventAction enabledCondition(@Nullable String enabledCondition) {
		action.setXmlEnabledCondition(enabledCondition);
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentSetDisabledEventAction putProperty(@Nonnull String k, @Nonnull String v) {
		action.getProperties().put(k, v);
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
