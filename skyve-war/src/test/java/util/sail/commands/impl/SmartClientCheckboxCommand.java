package util.sail.commands.impl;

import util.sail.commands.CheckboxCommand;

/**
 * Command representing a checkbox interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param value the value to set
 */
public record SmartClientCheckboxCommand(
		String locator,
		Boolean value) implements CheckboxCommand {
}
