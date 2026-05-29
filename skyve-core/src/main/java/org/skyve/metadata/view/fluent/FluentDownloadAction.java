package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.DownloadAction;

/**
 * Builds {@link DownloadAction} metadata using a fluent API.
 */
public class FluentDownloadAction extends FluentClassAction<FluentDownloadAction> {
	private DownloadAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link DownloadAction} instance.
	 */
	public FluentDownloadAction() {
		action = new DownloadAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentDownloadAction(DownloadAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentDownloadAction from(@SuppressWarnings("hiding") DownloadAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public DownloadAction get() {
		return action;
	}
}
