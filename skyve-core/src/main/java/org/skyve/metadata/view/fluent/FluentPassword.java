package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Password;

public class FluentPassword extends FluentChangeableInputWidget<FluentPassword> implements FluentAbsoluteWidth<FluentPassword> {
	private Password password = null;

	public FluentPassword() {
		password = new Password();
	}

	public FluentPassword(Password password) {
		this.password = password;
	}

	public FluentPassword from(@SuppressWarnings("hiding") Password password) {

		absoluteWidth(password, this);

		super.from(password);
		return this;
	}

	@Override
	public FluentPassword pixelWidth(int width) {
		password.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public Password get() {
		return password;
	}
}
