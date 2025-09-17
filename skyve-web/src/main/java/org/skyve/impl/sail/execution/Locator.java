package org.skyve.impl.sail.execution;

/**
 * Represents a UI element locator used in SC SAIL automation.
 * 
 * @author simeonsolomou
 */
public class Locator {

	private final String locator;
	private final InputType inputType;

	public enum InputType {
		CHECKBOX,
		COMBO,
		LOOKUP_DESCRIPTION,
		TEXT,
		RADIO
	}

	public Locator(String locator) {
		this.locator = locator;
		this.inputType = null;
	}

	public Locator(String locator, InputType inputType) {
		this.locator = locator;
		this.inputType = inputType;
	}

	public String getLocator() {
		return locator;
	}

	public InputType getInputType() {
		return inputType;
	}
}
