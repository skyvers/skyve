package util.sail.commands.impl;

import util.sail.commands.TabCommand;

/**
 * Command representing a tab interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param id the component identifier
 */
public record PrimeFacesTabCommand(
		String id) implements TabCommand {
}
