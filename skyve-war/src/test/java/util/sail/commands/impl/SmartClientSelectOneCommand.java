package util.sail.commands.impl;

import util.sail.commands.SelectOneCommand;

/**
 * Command representing a select one interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param id the component locator
 * @param index the index to select
 */
public record SmartClientSelectOneCommand(
		String locator,
		int index) implements SelectOneCommand {
}
