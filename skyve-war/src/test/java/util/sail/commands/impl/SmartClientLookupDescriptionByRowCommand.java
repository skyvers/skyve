package util.sail.commands.impl;

import util.sail.commands.LookupDescriptionByRowCommand;

/**
 * Command representing a lookup description by row interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param row the row to select
 */
public record SmartClientLookupDescriptionByRowCommand(
		String locator,
		int row) implements LookupDescriptionByRowCommand {
}
