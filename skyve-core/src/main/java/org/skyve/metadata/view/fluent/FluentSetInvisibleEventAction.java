package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
		BindUtil.copyAuthoredCondition(action.getXmlInvisibleCondition(), action.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
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
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentSetInvisibleEventAction invisibleCondition(@Nullable String invisibleCondition) {
		action.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentSetInvisibleEventAction visibleCondition(@Nullable String visibleCondition) {
		action.setXmlVisibleCondition(visibleCondition);
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
