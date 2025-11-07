package util.sail.commands.impl;

import util.sail.commands.ListGridButtonCommand;

/**
 * Command representing a list grid button interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param listGridId the list grid component identifier
 * @param buttonId the button component identifier
 * @param ajax whether the button triggers an AJAX request
 */
public record PrimeFacesListGridButtonCommand(
		String listGridId,
		String buttonId,
		boolean ajax) implements ListGridButtonCommand {
}
