package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link ServerSideActionEventAction} metadata using a fluent API.
 */
public class FluentServerSideActionEventAction extends FluentEventAction {
	private ServerSideActionEventAction action = null;

	/**
	 * Creates a fluent builder backed by a new {@link ServerSideActionEventAction} instance.
	 */
	public FluentServerSideActionEventAction() {
		action = new ServerSideActionEventAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentServerSideActionEventAction(ServerSideActionEventAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentServerSideActionEventAction from(@SuppressWarnings("hiding") ServerSideActionEventAction action) {
		actionName(action.getActionName());
		action.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Sets the server-side action name invoked by this event action.
	 *
	 * @param actionName
	 *            the server action name
	 * @return this builder
	 */
	public FluentServerSideActionEventAction actionName(String actionName) {
		action.setActionName(actionName);
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
	public @Nonnull FluentServerSideActionEventAction putProperty(@Nonnull String k, @Nonnull String v) {
		action.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the wrapped event action instance.
	 */
	@Override
	public ServerSideActionEventAction get() {
		return action;
	}
}
