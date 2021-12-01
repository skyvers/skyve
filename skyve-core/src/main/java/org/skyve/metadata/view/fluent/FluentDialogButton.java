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
		return this;
	}

	@Override
	public DialogButton get() {
		return button;
	}
}
