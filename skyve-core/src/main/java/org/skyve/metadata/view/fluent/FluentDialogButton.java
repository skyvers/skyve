package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.DialogButton;

public class FluentDialogButton extends FluentWidget {
	private DialogButton button = null;

	public FluentDialogButton() {
		button = new DialogButton();
	}

	public FluentDialogButton(DialogButton button) {
		this.button = button;
	}

	public FluentDialogButton from(@SuppressWarnings("hiding") DialogButton button) {

		displayName(button.getDisplayName());
		dialogName(button.getDialogName());
		command(button.getCommand());
		dialogHeight(button.getDialogHeight());
		dialogWidth(button.getDialogWidth());
		modalDialog(button.isModalDialog());
		invisibleConditionName(button.getInvisibleConditionName());
		disabledConditionName(button.getDisabledConditionName());

		button.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		return this;
	}

	public FluentDialogButton displayName(String displayName) {
		button.setDisplayName(displayName);
		return this;
	}

	public FluentDialogButton dialogName(String dialogName) {
		button.setDialogName(dialogName);
		return this;
	}

	public FluentDialogButton command(String command) {
		button.setCommand(command);
		return this;
	}

	// private Integer dialogWidth = Integer.valueOf(800);
	public FluentDialogButton dialogWidth(int dialogWidth) {
		button.setDialogWidth(Integer.valueOf(dialogWidth));
		return this;
	}

	// private Integer dialogHeight = Integer.valueOf(630);
	public FluentDialogButton dialogHeight(int dialogHeight) {
		button.setDialogHeight(Integer.valueOf(dialogHeight));
		return this;
	}

	// private Boolean modalDialog = Boolean.FALSE;
	public FluentDialogButton modalDialog(Boolean modalDialog) {
		button.setModalDialog(modalDialog);
		return this;
	}

	public FluentDialogButton invisibleConditionName(String invisibleConditionName) {
		button.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	public FluentDialogButton disabledConditionName(String disabledConditionName) {
		button.setDisabledConditionName(disabledConditionName);
		return this;
	}

	public FluentDialogButton addParameter(FluentParameter parameter) {
		button.getParameters().add(parameter.get());
		return this;
	}
	@Override
	public DialogButton get() {
		return button;
	}
}
