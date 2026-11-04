package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	protected void comment(@Nullable String comment) {
		indent().append("trace(\"").append(comment).append("\");").newline();
	}
	
	/**
	 * Starts a generated WebDriver test method for the supplied SAIL test heading.
	 *
	 * @param heading Test heading used to derive a Java-safe method name.
	 */
	@Override
	protected void startTest(@Nonnull String heading) {
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
	public void executeComment(@Nonnull Comment comment) {
		comment(comment.getComment());
	}
	
	/**
	 * Emits a pause call for a SAIL pause step.
	 *
	 * @param pause The pause step containing delay duration.
	 */
	@Override
	public void executePause(@Nonnull Pause pause) {
		indent().append("pause(\"").append(Long.toString(pause.getMillis())).append("\");").newline();
	}
	
	/**
	 * Emits a comment that {@code <upload/>} is not supported, because this generator has no WebDriver equivalent yet.
	 *
	 * @param upload the step being skipped
	 */
	@Override
	public void executeUpload(@Nonnull Upload upload) {
		comment("<upload/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <testMessage/>} is not supported, because this generator has no WebDriver equivalent yet.
	 *
	 * @param test the step being skipped
	 */
	@Override
	public void executeTestMessage(@Nonnull TestMessage test) {
		comment("<testMessage/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <testGridRows/>} is not supported, because this generator has no WebDriver equivalent yet.
	 *
	 * @param test the step being skipped
	 */
	@Override
	public void executeTestGridRows(@Nonnull TestGridRows test) {
		comment("<testGridRows/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <testWidget/>} is not supported, because this generator has no WebDriver equivalent yet.
	 *
	 * @param test the step being skipped
	 */
	@Override
	public void executeTestWidget(@Nonnull TestWidget test) {
		comment("<testWidget/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <navigateMenu/>} is not supported, because this generator has no WebDriver equivalent yet.
	 *
	 * @param menu the step being skipped
	 */
	@Override
	public void executeNavigateMenu(@Nonnull NavigateMenu menu) {
		comment("<navigateMenu/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <testMenu/>} is not supported, because this generator has no WebDriver equivalent yet.
	 *
	 * @param test the step being skipped
	 */
	@Override
	public void executeTestMenu(@Nonnull TestMenu test) {
		comment("<testMenu/> not supported by this generator");
	}

	/**
	 * Builds the final generated source including a test harness that calls each
	 * generated test method in declaration order.
	 *
	 * @return Generated source text.
	 */
	@Override
	public @Nonnull String toString() {
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
