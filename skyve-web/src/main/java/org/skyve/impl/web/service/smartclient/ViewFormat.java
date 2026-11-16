package org.skyve.impl.web.service.smartclient;

import org.skyve.metadata.view.TextOutput.Sanitisation;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

class ViewFormat {
	private String format;
	private boolean escape;
	private Sanitisation sanitise;
	
	ViewFormat(@Nonnull String format, boolean escape, @Nullable Sanitisation sanitise) {
		this.format = format;
		this.escape = escape;
		this.sanitise = sanitise;
	}
	
	@Nonnull String getFormat() {
		return format;
	}
	
	boolean isEscape() {
		return escape;
	}
	
	@Nullable Sanitisation getSanitise() {
		return sanitise;
	}
}
