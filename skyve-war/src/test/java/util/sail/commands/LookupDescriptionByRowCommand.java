package util.sail.commands;

/**
 * Marker interface for commands representing lookup description by row interactions in the UI test framework.
 * <p>
 * Implementations encapsulate parameters required to perform lookup description by row actions on different UI technologies.
 */
public interface LookupDescriptionByRowCommand {

	int row();
}
