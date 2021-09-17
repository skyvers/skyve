package org.skyve.impl.web.service.smartclient;

import org.skyve.metadata.view.TextOutput.Sanitisation;

public class ViewBinding {
	private boolean mutable = false;
	private boolean escape = false;
	private Sanitisation sanitise;
	private boolean instantiate; // Call BindUtil.instantiateAndGet()
	
	ViewBinding(boolean mutable, boolean escape, Sanitisation sanitise, boolean instantiate) {
		this.mutable = mutable;
		this.escape = escape;
		this.sanitise = sanitise;
		this.instantiate = instantiate;
	}
	
	void merge(@SuppressWarnings("hiding") boolean mutable,
				@SuppressWarnings("hiding") boolean escape,
				@SuppressWarnings("hiding") Sanitisation sanitise,
				@SuppressWarnings("hiding") boolean instantiate) {
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
		
		// Set instantiate - true trumps false
		if (instantiate && (! this.instantiate)) {
			this.instantiate = instantiate;
		}
	}
	
	public boolean isMutable() {
		return mutable;
	}
	
	public boolean isEscape() {
		return escape;
	}
	
	public Sanitisation getSanitise() {
		return sanitise;
	}

	public boolean isInstantiate() {
		return instantiate;
	}
}
