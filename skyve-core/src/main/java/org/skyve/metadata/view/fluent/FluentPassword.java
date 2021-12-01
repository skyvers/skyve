package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Password;

public class FluentPassword extends FluentWidget {
	private Password password = null;
	
	public FluentPassword() {
		password = new Password();
	}

	public FluentPassword(Password password) {
		this.password = password;
	}

	public FluentPassword from(@SuppressWarnings("hiding") Password password) {
		return this;
	}
	
	@Override
	public Password get() {
		return password;
	}
}
