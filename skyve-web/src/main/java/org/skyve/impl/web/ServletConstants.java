package org.skyve.impl.web;

public class ServletConstants {
	/**
	 * Prevent instantiation.
	 */
	private ServletConstants() {
		// do nothing
	}

	public static final String GET_METHOD = "GET";
	public static final String PUSH_ACTION_NAME = "_PUSH";
	/**
	 * The action a SmartClient inline data grid's New button sends. It starts with an underscore so that it
	 * cannot collide with a custom action class name, such as one called {@code Add}.
	 */
	public static final String INLINE_ADD_ACTION_NAME = "_ADD";
}
