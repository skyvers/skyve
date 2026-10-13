package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link ToggleVisibilityEventAction} metadata using a fluent API.
 */
public class FluentToggleVisibilityEventAction extends FluentEventAction {
	private ToggleVisibilityEventAction action = null;

	/**
	 * Creates a fluent builder backed by a new {@link ToggleVisibilityEventAction} instance.
	 */
	public FluentToggleVisibilityEventAction() {
		action = new ToggleVisibilityEventAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentToggleVisibilityEventAction(ToggleVisibilityEventAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentToggleVisibilityEventAction from(@SuppressWarnings("hiding") ToggleVisibilityEventAction action) {
		binding(action.getBinding());
		action.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Sets the target binding toggled by this event action.
	 *
	 * @param binding
	 *            the target binding expression
	 * @return this builder
	 */
	public FluentToggleVisibilityEventAction binding(String binding) {
		action.setBinding(binding);
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
	public @Nonnull FluentToggleVisibilityEventAction putProperty(@Nonnull String k, @Nonnull String v) {
		action.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the wrapped event action instance.
	 */
	@Override
	public ToggleVisibilityEventAction get() {
		return action;
	}
}
