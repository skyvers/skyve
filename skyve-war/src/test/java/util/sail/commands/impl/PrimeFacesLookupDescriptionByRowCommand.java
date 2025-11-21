package util.sail.commands.impl;

import util.sail.commands.LookupDescriptionByRowCommand;

/**
 * Command representing a lookup description by row interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param id the component identifier
 * @param row the row to select
 */
public record PrimeFacesLookupDescriptionByRowCommand(
		String id,
		int row) implements LookupDescriptionByRowCommand {
}
