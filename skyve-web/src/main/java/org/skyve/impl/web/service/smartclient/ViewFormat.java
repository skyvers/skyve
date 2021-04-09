package org.skyve.impl.web.service.smartclient;

import org.skyve.metadata.view.TextOutput.Sanitisation;

class ViewFormat {
	private String format;
	private boolean escape;
	private Sanitisation sanitise;
	
	ViewFormat(String format, boolean escape, Sanitisation sanitise) {
		this.format = format;
		this.escape = escape;
		this.sanitise = sanitise;
	}
	
	String getFormat() {
		return format;
	}
	
	boolean isEscape() {
		return escape;
	}
	
	Sanitisation getSanitise() {
		return sanitise;
	}
}
