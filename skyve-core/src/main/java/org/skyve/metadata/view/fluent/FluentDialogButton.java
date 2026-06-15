package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.DialogButton;

/**
 * Builds {@link DialogButton} metadata for opening configured dialogs.
 */
public class FluentDialogButton extends FluentWidget {
	private DialogButton button = null;

	/**
	 * Creates a builder backed by a new {@link DialogButton}.
	 */
	public FluentDialogButton() {
		button = new DialogButton();
	}

	/**
	 * Creates a builder backed by the supplied {@link DialogButton}.
	 *
	 * @param button
	 *            the metadata instance to mutate
	 */
	public FluentDialogButton(DialogButton button) {
		this.button = button;
	}

	/**
	 * Copies dialog button state, including configured invocation parameters.
	 *
	 * @param button
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentDialogButton from(@SuppressWarnings("hiding") DialogButton button) {

		displayName(button.getDisplayName());
		dialogName(button.getDialogName());
		command(button.getCommand());
		Integer i = button.getDialogHeight();
		if (i != null) {
			dialogHeight(i.intValue());
		}
		i = button.getDialogWidth();
		if (i != null) {
			dialogWidth(i.intValue());
		}
		Boolean b = button.isModalDialog();
		if (b != null) {
			modalDialog(b.booleanValue());
		}
		invisibleConditionName(button.getInvisibleConditionName());
		disabledConditionName(button.getDisabledConditionName());

		button.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		return this;
	}

	/**
	 * Sets the button display label.
	 *
	 * @param displayName
	 *            the button label
	 * @return this builder
	 */
	public FluentDialogButton displayName(String displayName) {
		button.setDisplayName(displayName);
		return this;
	}

	/**
	 * Sets the dialog name opened by the button.
	 *
	 * @param dialogName
	 *            the dialog identifier
	 * @return this builder
	 */
	public FluentDialogButton dialogName(String dialogName) {
		button.setDialogName(dialogName);
		return this;
	}

	/**
	 * Sets the command executed when the dialog button is invoked.
	 *
	 * @param command
	 *            the command identifier
	 * @return this builder
	 */
	public FluentDialogButton command(String command) {
		button.setCommand(command);
		return this;
	}


	/**
	 * Sets the dialog width in pixels.
	 *
	 * @param dialogWidth
	 *            the dialog width
	 * @return this builder
	 */
	public FluentDialogButton dialogWidth(int dialogWidth) {
		button.setDialogWidth(Integer.valueOf(dialogWidth));
		return this;
	}


	/**
	 * Sets the dialog height in pixels.
	 *
	 * @param dialogHeight
	 *            the dialog height
	 * @return this builder
	 */
	public FluentDialogButton dialogHeight(int dialogHeight) {
		button.setDialogHeight(Integer.valueOf(dialogHeight));
		return this;
	}

	/**
	 * Sets whether the dialog is modal.
	 *
	 * @param modalDialog
	 *            {@code true} to require modal interaction
	 * @return this builder
	 */
	public FluentDialogButton modalDialog(boolean modalDialog) {
		button.setModalDialog(modalDialog ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the condition name controlling button visibility.
	 *
	 * @param invisibleConditionName
	 *            the visibility condition name
	 * @return this builder
	 */
	public FluentDialogButton invisibleConditionName(String invisibleConditionName) {
		button.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Sets the condition name controlling button enabled state.
	 *
	 * @param disabledConditionName
	 *            the disable condition name
	 * @return this builder
	 */
	public FluentDialogButton disabledConditionName(String disabledConditionName) {
		button.setDisabledConditionName(disabledConditionName);
		return this;
	}

	/**
	 * Appends a command parameter for dialog invocation.
	 *
	 * @param parameter
	 *            the parameter builder whose metadata is appended
	 * @return this builder
	 */
	public FluentDialogButton addParameter(FluentParameter parameter) {
		button.getParameters().add(parameter.get());
		return this;
	}
	
	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped dialog-button metadata
	 */
	@Override
	public DialogButton get() {
		return button;
	}
}
