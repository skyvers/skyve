package util.sail.commands.impl;

import util.sail.commands.TextCommand;

/**
 * Command representing a text interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param value the value to enter
 * @param keyPresses true if entering value key presses
 */
public record SmartClientTextCommand(
		String locator,
		String value,
		boolean keyPresses) implements TextCommand {
}
