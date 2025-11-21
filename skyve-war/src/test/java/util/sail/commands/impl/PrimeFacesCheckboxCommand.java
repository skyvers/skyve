package util.sail.commands.impl;

import util.sail.commands.CheckboxCommand;

/**
 * Command representing a checkbox interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param id the component identifier
 * @param value the value to set
 */
public record PrimeFacesCheckboxCommand(
		String id,
		Boolean value) implements CheckboxCommand {
}
