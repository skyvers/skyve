package util.sail.commands.impl;

import util.sail.commands.DataGridSelectCommand;

/**
 * Command representing a data grid select interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param dataGridId the data grid component identifier
 * @param row the row to select
 */
public record PrimeFacesDataGridSelectCommand(
		String dataGridId,
		int row) implements DataGridSelectCommand {
}
