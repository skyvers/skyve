package util.sail.commands.impl;

import util.sail.commands.ListGridSelectCommand;

/**
 * Command representing a list grid select interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param listGridId the data grid component identifier
 * @param row the row to select
 */
public record PrimeFacesListGridSelectCommand(
		String listGridId,
		int row) implements ListGridSelectCommand {
}
