package util.sail.commands.impl;

import util.sail.commands.RadioCommand;

/**
 * Command representing a radio interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param index the index to select
 */
public record SmartClientRadioCommand(
		String locator,
		int index) implements RadioCommand {
}
