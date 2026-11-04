package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	protected void command(@Nonnull String command, @Nullable String parameter1, @Nullable String parameter2) {
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
	protected void command(@Nonnull String command, @Nullable String parameter1) {
		command(command, parameter1, null);
	}

	/**
	 * Writes a single-command Selenese row.
	 *
	 * @param command Command name.
	 */
	protected void command(@Nonnull String command) {
		command(command, null, null);
	}

	/**
	 * Writes an HTML comment into the generated Selenese output.
	 *
	 * @param comment Comment text.
	 */
	protected void comment(@Nullable String comment) {
		indent().append("<!-- ").append(comment).append(" -->").newline();
	}
	
	/**
	 * Starts a Selenese test table with the supplied heading.
	 *
	 * @param heading Heading text shown in the table header.
	 */
	@Override
	protected void startTest(@Nonnull String heading) {
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
	public void executeComment(@Nonnull Comment comment) {
		comment(comment.getComment());
	}
	
	/**
	 * Ignores pause steps because Selenese output does not support direct timing pauses here.
	 *
	 * @param pause The pause step.
	 */
	@Override
	public void executePause(@Nonnull Pause pause) {
		// do nothing - we can't pause easily in Selenese
	}

	/**
	 * Emits a comment that {@code <upload/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param upload the step being skipped
	 */
	@Override
	public void executeUpload(@Nonnull Upload upload) {
		comment("<upload/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <testMessage/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param test the step being skipped
	 */
	@Override
	public void executeTestMessage(@Nonnull TestMessage test) {
		comment("<testMessage/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <testGridRows/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param test the step being skipped
	 */
	@Override
	public void executeTestGridRows(@Nonnull TestGridRows test) {
		comment("<testGridRows/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <testWidget/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param test the step being skipped
	 */
	@Override
	public void executeTestWidget(@Nonnull TestWidget test) {
		comment("<testWidget/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <navigateMenu/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param menu the step being skipped
	 */
	@Override
	public void executeNavigateMenu(@Nonnull NavigateMenu menu) {
		comment("<navigateMenu/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <testMenu/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param test the step being skipped
	 */
	@Override
	public void executeTestMenu(@Nonnull TestMenu test) {
		comment("<testMenu/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <switchMode/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param switchMode the step being skipped
	 */
	@Override
	public void executeSwitchMode(@Nonnull SwitchMode switchMode) {
		comment("<switchMode/> not supported by this generator");
	}
}
