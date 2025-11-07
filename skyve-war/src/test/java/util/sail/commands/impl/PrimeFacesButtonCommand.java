package util.sail.commands.impl;

import util.sail.commands.ButtonCommand;

/**
 * Command representing a button interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param id the component identifier
 * @param ajax whether the button triggers an AJAX request
 * @param confirm whether confirmation is required after clicking
 */
public record PrimeFacesButtonCommand(
		String id,
		boolean ajax,
		boolean confirm) implements ButtonCommand {
}
