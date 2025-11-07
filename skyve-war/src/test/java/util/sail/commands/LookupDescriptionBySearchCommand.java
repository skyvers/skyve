package util.sail.commands;

/**
 * Marker interface for commands representing lookup description by search interactions in the UI test framework.
 * <p>
 * Implementations encapsulate parameters required to perform lookup description by search actions on different UI technologies.
 */
public interface LookupDescriptionBySearchCommand {

	String search();
}
