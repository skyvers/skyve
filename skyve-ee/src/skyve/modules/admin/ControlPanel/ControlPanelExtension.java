package modules.admin.ControlPanel;

import java.io.PrintWriter;
import java.io.StringWriter;

import modules.admin.domain.ControlPanel;

public class ControlPanelExtension extends ControlPanel {
	private static final long serialVersionUID = -6204655500999983605L;

	public void trapException(Exception e) {
		StringWriter sw = new StringWriter(512);
		PrintWriter pw = new PrintWriter(sw);
		e.printStackTrace(pw);
		setResults(sw.toString());
	}
	
	/**
	 * Overriden to escape {, < & >.
	 * Add a new line out the front to line up all lines to the left of the blurb.
	 */
	@Override
	public void setResults(String results) {
		super.setResults('\n' + results.replace("{", "\\{").replace("<", "&lt;").replace(">", "&gt;"));
	}
}
