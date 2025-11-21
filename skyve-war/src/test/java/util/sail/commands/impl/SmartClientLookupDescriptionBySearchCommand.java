package util.sail.commands.impl;

import util.sail.commands.LookupDescriptionBySearchCommand;

/**
 * Command representing a lookup description by search interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param search the value to search
 */
public record SmartClientLookupDescriptionBySearchCommand(
		String locator,
		String search) implements LookupDescriptionBySearchCommand {
}
