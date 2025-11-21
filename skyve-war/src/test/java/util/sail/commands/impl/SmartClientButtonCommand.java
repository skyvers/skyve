package util.sail.commands.impl;

import util.sail.commands.ButtonCommand;

/**
 * Command representing a button interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param confirm whether confirmation is required after clicking
 */
public record SmartClientButtonCommand(
		String locator,
		boolean confirm) implements ButtonCommand {
}
