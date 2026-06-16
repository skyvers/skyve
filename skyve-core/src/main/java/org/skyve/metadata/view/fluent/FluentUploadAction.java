package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.UploadAction;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentUploadAction(@Nonnull UploadAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentUploadAction from(@SuppressWarnings("hiding") @Nonnull UploadAction action) {
		super.from(action);
		ContentCapture capture = action.getCapture();
		if (capture != null) {
			capture(capture);
		}
		return this;
	}

	/**
	 * Sets the upload capture affordance requested by this action.
	 *
	 * @param capture the capture affordance, or {@code null} to use the action
	 *        default
	 * @return this builder
	 */
	public @Nonnull FluentUploadAction capture(@Nullable ContentCapture capture) {
		action.setCapture(capture);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 *
	 * @return the mutable backing action; never {@code null}
	 */
	@Override
	public @Nonnull UploadAction get() {
		return action;
	}
}
