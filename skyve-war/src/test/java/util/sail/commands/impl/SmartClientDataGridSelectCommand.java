package util.sail.commands.impl;

import util.sail.commands.DataGridSelectCommand;

/**
 * Command representing a data grid select interaction in a SmartClient UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the SmartClient-specific implementation of the test framework.
 *
 * @param locator the component locator
 * @param row the row to select
 */
public record SmartClientDataGridSelectCommand(
		String locator,
		int row) implements DataGridSelectCommand {
}
