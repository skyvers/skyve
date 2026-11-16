package util.sail.commands.impl;

import util.sail.commands.ButtonCommand;

/**
 * Command representing a button interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param confirm how to answer a confirmation after clicking: null or true answers yes if one shows, false answers no and one must show
 */
public record SmartClientButtonCommand(
		String locator,
		Boolean confirm) implements ButtonCommand {
}
