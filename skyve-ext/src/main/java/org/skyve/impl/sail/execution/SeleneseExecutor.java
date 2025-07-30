package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Pause;

public abstract class SeleneseExecutor<T extends AutomationContext> extends ScriptExecutor<T> {
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
	
	protected void command(String command, String parameter1) {
		command(command, parameter1, null);
	}

	protected void command(String command) {
		command(command, null, null);
	}

	protected void comment(String comment) {
		indent().append("<!-- ").append(comment).append(" -->").newline();
	}
	
	@Override
	protected void startTest(String heading) {
		indent().append("<table>").newline();
		in().indent().append("<thead>").newline();
		in().indent().append("<tr><th>").append(heading).append("</th></tr>").newline();
		out().indent().append("</thead>").newline();
		indent().append("<tbody>").newline().in();
	}
	
	@Override
	protected void endTest() {
		out().indent().append("</tbody>").newline();
		out().indent().append("</table>").newline();
	}
	
	@Override
	public void executeComment(Comment comment) {
		comment(comment.getComment());
	}
	
	@Override
	public void executePause(Pause pause) {
		// do nothing - we can't pause easily in Selenese
	}
}
