package util.sail.commands.impl;

import util.sail.commands.ListGridButtonCommand;

/**
 * Command representing a list grid button interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component identifier
 * @param confirm whether confirmation is required after clicking
 */
public record SmartClientListGridButtonCommand(
		String locator,
		boolean confirm) implements ListGridButtonCommand {
}
