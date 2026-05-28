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

	/**
	 * Categorises supported SmartClient input widgets for locator interaction handling.
	 */
	public enum InputType {
		CHECKBOX,
		COMBO,
		LOOKUP_DESCRIPTION,
		TEXT,
		RADIO
	}

	/**
	 * Creates a locator without input type hints.
	 */
	public Locator(String locator) {
		this.locator = locator;
		this.inputType = null;
	}

	/**
	 * Creates a locator with optional input type metadata.
	 */
	public Locator(String locator, InputType inputType) {
		this.locator = locator;
		this.inputType = inputType;
	}

	/**
	 * Returns the concrete selector string used by automation steps.
	 */
	public String getLocator() {
		return locator;
	}

	/**
	 * Returns the optional input type hint for this locator.
	 */
	public InputType getInputType() {
		return inputType;
	}
}
