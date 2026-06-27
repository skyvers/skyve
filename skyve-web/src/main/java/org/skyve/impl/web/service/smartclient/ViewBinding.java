package org.skyve.impl.web.service.smartclient;

import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Implements internal web-module behavior for this Skyve runtime concern.
 */
public class ViewBinding {
	private boolean mutable = false;
	private boolean escape = false;
	private Sanitisation sanitise;
	private boolean instantiate; // Call BindUtil.instantiateAndGet()
	
	/**
	 * Creates a view binding state used while collecting widget binding behaviour.
	 *
	 * @param mutable Whether the binding can be written during apply operations.
	 * @param escape Whether the value should be escaped before rendering.
	 * @param sanitise The sanitisation level to apply, if any.
	 * @param instantiate Whether missing binding paths should be instantiated.
	 */
	ViewBinding(boolean mutable, boolean escape, Sanitisation sanitise, boolean instantiate) {
		this.mutable = mutable;
		this.escape = escape;
		this.sanitise = sanitise;
		this.instantiate = instantiate;
	}
	
	/**
	 * Merges binding flags from another binding declaration.
	 *
	 * <p>Merge rules are monotonic: boolean flags are promoted by true values,
	 * and sanitisation adopts the highest ordinal level.
	 *
	 * @param mutable Incoming mutable flag.
	 * @param escape Incoming escape flag.
	 * @param sanitise Incoming sanitisation level.
	 * @param instantiate Incoming instantiate flag.
	 */
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
	
	/**
	 * Indicates whether this binding participates in mutable apply operations.
	 */
	public boolean isMutable() {
		return mutable;
	}
	
	/**
	 * Indicates whether this binding should be escaped before render.
	 */
	public boolean isEscape() {
		return escape;
	}
	
	/**
	 * Returns the configured sanitisation policy for this binding.
	 */
	public Sanitisation getSanitise() {
		return sanitise;
	}

	/**
	 * Indicates whether missing intermediate bindings should be instantiated during resolution.
	 */
	public boolean isInstantiate() {
		return instantiate;
	}
}
