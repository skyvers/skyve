package util.sail.commands.impl;

import util.sail.commands.DataGridButtonCommand;

/**
 * Command representing a data grid button interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param dataGridId the data grid component identifier
 * @param buttonId the button component identifier
 * @param ajax whether the button triggers an AJAX request
 */
public record PrimeFacesDataGridButtonCommand(
		String dataGridId,
		String buttonId,
		boolean ajax) implements DataGridButtonCommand {
}
