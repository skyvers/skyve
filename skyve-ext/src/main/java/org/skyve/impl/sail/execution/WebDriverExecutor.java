package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Pause;

/**
 * Base executor that generates WebDriver-style Java test methods from SAIL scripts.
 */
public abstract class WebDriverExecutor<T extends AutomationContext> extends ScriptExecutor<T> {
	/**
	 * Collected generated test method suffixes, invoked by the harness test method.
	 */
	private List<String> testMethodNames = new ArrayList<>();
	
	/**
	 * Emits a trace comment statement into generated test code.
	 *
	 * @param comment Comment text.
	 */
	protected void comment(String comment) {
		indent().append("trace(\"").append(comment).append("\");").newline();
	}
	
	/**
	 * Starts a generated WebDriver test method for the supplied SAIL test heading.
	 *
	 * @param heading Test heading used to derive a Java-safe method name.
	 */
	@Override
	protected void startTest(String heading) {
		String testMethodName = BindUtil.toJavaTypeIdentifier(heading);
		testMethodNames.add(testMethodName);
		
		indent().append("/**").newline();
		indent().append(" * ").append(heading).newline();
		indent().append(" */").newline();
		indent().append("private void test").append(testMethodName).append("() {").newline().in();
	}
	
	/**
	 * Ends the current generated WebDriver test method.
	 */
	@Override
	protected void endTest() {
		out().indent().append("}").newline().newline();
	}
	
	/**
	 * Emits a trace comment from a SAIL comment step.
	 *
	 * @param comment The comment step to render.
	 */
	@Override
	public void executeComment(Comment comment) {
		comment(comment.getComment());
	}
	
	/**
	 * Emits a pause call for a SAIL pause step.
	 *
	 * @param pause The pause step containing delay duration.
	 */
	@Override
	public void executePause(Pause pause) {
		indent().append("pause(\"").append(Long.toString(pause.getMillis())).append("\");").newline();
	}
	
	/**
	 * Builds the final generated source including a test harness that calls each
	 * generated test method in declaration order.
	 *
	 * @return Generated source text.
	 */
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
