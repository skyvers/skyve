package org.skyve.impl.sail.execution.sc;

/**
 * Represents a UI element locator used in SmartClient SAIL automation.
 * <p>
 * The {@code inputType} is optional and should be provided when the input type context
 * is needed to correctly manipulate the locator during interactions.
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
