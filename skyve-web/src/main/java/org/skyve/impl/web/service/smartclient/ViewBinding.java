package org.skyve.impl.web.service.smartclient;

import org.skyve.metadata.view.TextOutput.Sanitisation;

class ViewBinding {
	private boolean mutable = false;
	private boolean escape = false;
	private Sanitisation sanitise;
	
	ViewBinding(boolean mutable, boolean escape, Sanitisation sanitise) {
		this.mutable = mutable;
		this.escape = escape;
		this.sanitise = sanitise;
	}
	
	void merge(@SuppressWarnings("hiding") boolean mutable,
				@SuppressWarnings("hiding") boolean escape,
				@SuppressWarnings("hiding") Sanitisation sanitise) {
		// Set mutable - true trumps false
		if (mutable && (! this.mutable)) {
			this.mutable = mutable;
		}

		// Set escape - true trumps false
		if (escape && (! this.escape)) {
			this.escape = escape;
		}
		
		// Set sanitise - higher ordinal trumps lower
		if (sanitise != null) {
			if (this.sanitise == null) {
				this.sanitise = sanitise;
			}
			else if (sanitise.ordinal() > this.sanitise.ordinal()) {
				this.sanitise = sanitise;
			}
		}
	}
	
	boolean isMutable() {
		return mutable;
	}
	
	boolean isEscape() {
		return escape;
	}
	
	Sanitisation getSanitise() {
		return sanitise;
	}
}
