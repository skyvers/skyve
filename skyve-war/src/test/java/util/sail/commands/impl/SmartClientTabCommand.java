package util.sail.commands.impl;

import util.sail.commands.TabCommand;

/**
 * Command representing a tab interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 */
public record SmartClientTabCommand(
		String locator) implements TabCommand {
}
