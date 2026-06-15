package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.UploadAction;

/**
 * Builds {@link UploadAction} metadata using a fluent API.
 */
public class FluentUploadAction extends FluentClassAction<FluentUploadAction> {
	private UploadAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link UploadAction} instance.
	 */
	public FluentUploadAction() {
		action = new UploadAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentUploadAction(UploadAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentUploadAction from(@SuppressWarnings("hiding") UploadAction action) {
		super.from(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public UploadAction get() {
		return action;
	}
}
