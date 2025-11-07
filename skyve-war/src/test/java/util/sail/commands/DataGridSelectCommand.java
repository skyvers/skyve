package util.sail.commands;

/**
 * Marker interface for commands representing data grid select interactions in the UI test framework.
 * <p>
 * Implementations encapsulate parameters required to perform data grid select actions on different UI technologies.
 */
public interface DataGridSelectCommand {

	int row();
}
