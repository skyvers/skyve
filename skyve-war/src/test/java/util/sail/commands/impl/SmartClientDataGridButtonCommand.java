package util.sail.commands.impl;

import util.sail.commands.DataGridButtonCommand;

/**
 * Command representing a data grid button interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param confirm whether confirmation is required after clicking
 */
public record SmartClientDataGridButtonCommand(
		String locator,
		boolean confirm) implements DataGridButtonCommand {
}
