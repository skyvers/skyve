package util.sail.commands;

/**
 * Marker interface for commands representing text interactions in the UI test framework.
 * <p>
 * Implementations encapsulate parameters required to perform text actions on different UI technologies.
 */
public interface TextCommand {

	String value();

	boolean keyPresses();
}
