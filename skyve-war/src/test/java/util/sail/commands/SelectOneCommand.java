package util.sail.commands;

/**
 * Marker interface for commands representing select one interactions in the UI test framework.
 * <p>
 * Implementations encapsulate parameters required to perform select one actions on different UI technologies.
 */
public interface SelectOneCommand {

	int index();
}
