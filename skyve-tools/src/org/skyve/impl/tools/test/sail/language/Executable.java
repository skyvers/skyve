package org.skyve.impl.tools.test.sail.language;

public interface Executable {
	public void execute(StringBuilder script, int indentationDepth);
	
	public default void indent(StringBuilder script, int indentationDepth) {
		for (int i = 0; i < indentationDepth; i++) {
			script.append('\t');
		}
	}
	
	public default void startTest(String heading, StringBuilder script, int indentationDepth) {
		indent(script, indentationDepth);
		script.append("<table>\n");
		indent(script, indentationDepth + 1);
		script.append("<thead>\n");
		indent(script, indentationDepth + 2);
		script.append("<tr><th>").append(heading).append("</th></tr>\n");
		indent(script, indentationDepth + 1);
		script.append("</thead>\n");
		indent(script, indentationDepth + 1);
		script.append("<tbody>\n");
	}
	
	public default void endTest(StringBuilder script, int indentationDepth) {
		indent(script, indentationDepth + 1);
		script.append("</tbody>\n");
		indent(script, indentationDepth);
		script.append("</table>\n");
	}
}
