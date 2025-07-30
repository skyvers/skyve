package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Pause;

public abstract class WebDriverExecutor<T extends AutomationContext> extends ScriptExecutor<T> {
	private List<String> testMethodNames = new ArrayList<>();
	
	protected void comment(String comment) {
		indent().append("trace(\"").append(comment).append("\");").newline();
	}
	
	@Override
	protected void startTest(String heading) {
		String testMethodName = BindUtil.toJavaTypeIdentifier(heading);
		testMethodNames.add(testMethodName);
		
		indent().append("/**").newline();
		indent().append(" * ").append(heading).newline();
		indent().append(" */").newline();
		indent().append("private void test").append(testMethodName).append("() {").newline().in();
	}
	
	@Override
	protected void endTest() {
		out().indent().append("}").newline().newline();
	}
	
	@Override
	public void executeComment(Comment comment) {
		comment(comment.getComment());
	}
	
	@Override
	public void executePause(Pause pause) {
		indent().append("pause(\"").append(Long.toString(pause.getMillis())).append("\");").newline();
	}
	
	@Override
	public String toString() {
		indent().append("/**").newline();
		indent().append(" * Test Harness").newline();
		indent().append(" */").newline();
		indent().append("@Test").newline();
		indent().append("public void test() {").newline().in();

		for (String testMethodName : testMethodNames) {
			indent().append("test").append(testMethodName).append("();").newline();
		}
		
		out().indent().append("}").newline();
		
		return super.toString();
	}
}
