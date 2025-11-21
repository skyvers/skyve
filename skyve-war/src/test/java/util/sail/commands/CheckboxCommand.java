package util.sail.commands;

/**
 * Marker interface for commands representing checkbox interactions in the UI test framework.
 * <p>
 * Implementations encapsulate parameters required to perform checkbox actions on different UI technologies.
 */
public interface CheckboxCommand {

	Boolean value();
}
