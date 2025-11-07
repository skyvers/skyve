package util.sail.commands.impl;

import util.sail.commands.SelectOneCommand;

/**
 * Command representing a select one interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param id the component identifier
 * @param index the index to select
 */
public record PrimeFacesSelectOneCommand(
		String id,
		int index) implements SelectOneCommand {
}
