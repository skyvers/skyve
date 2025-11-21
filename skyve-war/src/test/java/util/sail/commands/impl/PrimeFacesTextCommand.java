package util.sail.commands.impl;

import util.sail.commands.TextCommand;

/**
 * Command representing a text interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param id the component identifier
 * @param value the value to enter
 * @param keyPresses true if entering value key presses
 */
public record PrimeFacesTextCommand(
		String id,
		String value,
		boolean keyPresses) implements TextCommand {
}
