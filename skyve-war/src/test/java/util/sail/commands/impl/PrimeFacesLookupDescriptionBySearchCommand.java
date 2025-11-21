package util.sail.commands.impl;

import util.sail.commands.LookupDescriptionBySearchCommand;

/**
 * Command representing a lookup description by search interaction in a PrimeFaces UI.
 * <p>
 * Contains parameters for identification and interaction.
 * Used by the PrimeFaces-specific implementation of the test framework.
 *
 * @param id the component identifier
 * @param search the value to search
 */
public record PrimeFacesLookupDescriptionBySearchCommand(
		String id,
		String search) implements LookupDescriptionBySearchCommand {

}
