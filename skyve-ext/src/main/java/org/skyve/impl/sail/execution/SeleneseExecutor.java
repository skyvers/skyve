package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Pause;

/**
 * Base executor that renders Selenese-compatible table commands for SAIL scripts.
 */
public abstract class SeleneseExecutor<T extends AutomationContext> extends ScriptExecutor<T> {
	/**
	 * Writes a full three-column Selenese command row.
	 *
	 * @param command Command name.
	 * @param parameter1 First command parameter.
	 * @param parameter2 Second command parameter.
	 */
	protected void command(String command, String parameter1, String parameter2) {
		indent().append("<tr><td>").append(command).append("</td><td>");
		if (parameter1 != null) {
			append(parameter1);
		}
		append("</td><td>");
		if (parameter2 != null) {
			append(parameter2);
		}
		append("</td></tr>").newline();
	}
	
	/**
	 * Writes a two-column Selenese command row.
	 *
	 * @param command Command name.
	 * @param parameter1 First command parameter.
	 */
	protected void command(String command, String parameter1) {
		command(command, parameter1, null);
	}

	/**
	 * Writes a single-command Selenese row.
	 *
	 * @param command Command name.
	 */
	protected void command(String command) {
		command(command, null, null);
	}

	/**
	 * Writes an HTML comment into the generated Selenese output.
	 *
	 * @param comment Comment text.
	 */
	protected void comment(String comment) {
		indent().append("<!-- ").append(comment).append(" -->").newline();
	}
	
	/**
	 * Starts a Selenese test table with the supplied heading.
	 *
	 * @param heading Heading text shown in the table header.
	 */
	@Override
	protected void startTest(String heading) {
		indent().append("<table>").newline();
		in().indent().append("<thead>").newline();
		in().indent().append("<tr><th>").append(heading).append("</th></tr>").newline();
		out().indent().append("</thead>").newline();
		indent().append("<tbody>").newline().in();
	}
	
	/**
	 * Closes the Selenese test table body and table elements.
	 */
	@Override
	protected void endTest() {
		out().indent().append("</tbody>").newline();
		out().indent().append("</table>").newline();
	}
	
	/**
	 * Emits a Selenese comment row from the supplied SAIL comment step.
	 *
	 * @param comment The SAIL comment step to render.
	 */
	@Override
	public void executeComment(Comment comment) {
		comment(comment.getComment());
	}
	
	/**
	 * Ignores pause steps because Selenese output does not support direct timing pauses here.
	 *
	 * @param pause The pause step.
	 */
	@Override
	public void executePause(Pause pause) {
		// do nothing - we can't pause easily in Selenese
	}
}
