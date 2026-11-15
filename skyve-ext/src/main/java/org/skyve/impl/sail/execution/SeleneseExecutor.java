package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.CaptureCell;
import org.skyve.metadata.sail.language.step.CaptureTitle;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridLink;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridMove;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool;
import org.skyve.metadata.sail.language.step.interaction.grids.ListMembershipMove;
import org.skyve.metadata.sail.language.step.interaction.navigation.CloseWindow;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateBack;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.Refresh;
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

	/**
	 * Emits a comment that {@code <listGridFilter/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param filter the step being skipped
	 */
	@Override
	public void executeListGridFilter(@Nonnull ListGridFilter filter) {
		comment("<listGridFilter/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <listGridClearFilter/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param clear the step being skipped
	 */
	@Override
	public void executeListGridClearFilter(@Nonnull ListGridClearFilter clear) {
		comment("<listGridClearFilter/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <listGridSort/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param sort the step being skipped
	 */
	@Override
	public void executeListGridSort(@Nonnull ListGridSort sort) {
		comment("<listGridSort/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <captureValue/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param capture the step being skipped
	 */
	@Override
	public void executeCaptureValue(@Nonnull CaptureValue capture) {
		comment("<captureValue/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <captureCell/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param capture the step being skipped
	 */
	@Override
	public void executeCaptureCell(@Nonnull CaptureCell capture) {
		comment("<captureCell/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <captureTitle/>} is not supported, because this generator has no Selenese equivalent yet.
	 *
	 * @param capture the step being skipped
	 */
	@Override
	public void executeCaptureTitle(@Nonnull CaptureTitle capture) {
		comment("<captureTitle/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <dataGridLink/>} is not supported, because this generator has no Selenese equivalent yet.
	 * The context is left as it is.
	 *
	 * @param link the step being skipped
	 */
	@Override
	public void executeDataGridLink(@Nonnull DataGridLink link) {
		comment("<dataGridLink/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <dataGridMove/>} is not supported, because this generator has no Selenese equivalent yet.
	 * The context is left as it is.
	 *
	 * @param move the step being skipped
	 */
	@Override
	public void executeDataGridMove(@Nonnull DataGridMove move) {
		comment("<dataGridMove/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <listMembershipMove/>} is not supported, because this generator has no Selenese equivalent
	 * yet. The context is left as it is.
	 *
	 * @param move the step being skipped
	 */
	@Override
	public void executeListMembershipMove(@Nonnull ListMembershipMove move) {
		comment("<listMembershipMove/> not supported by this generator");
	}

	/**
	 * Sets the contexts aside for a new window, as the interpreted executors do, and for {@code window="new"} emits a
	 * comment that the generated code does not follow the new window (the comment has no quotes, as it goes into a
	 * generated string literal). A link in the same window is handled as before.
	 *
	 * @param link the step
	 */
	@Override
	public void executeNavigateLink(@Nonnull NavigateLink link) {
		super.executeNavigateLink(link);
		if (NavigateLink.Window.newWindow == link.getWindow()) {
			comment("<navigateLink window=new/> not supported by this generator: the generated code does not follow the new window");
		}
	}

	/**
	 * Emits a comment that {@code <navigateBack/>} is not supported, because this generator has no Selenese equivalent yet.
	 * The top context loses its document and view type, as the interpreted executors' does.
	 *
	 * @param back the step being skipped
	 */
	@Override
	public void executeNavigateBack(@Nonnull NavigateBack back) {
		super.executeNavigateBack(back);
		comment("<navigateBack/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <refresh/>} is not supported, because this generator has no Selenese equivalent yet.
	 * The context is left as it is.
	 *
	 * @param refresh the step being skipped
	 */
	@Override
	public void executeRefresh(@Nonnull Refresh refresh) {
		comment("<refresh/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <closeWindow/>} is not supported, because this generator has no Selenese equivalent yet.
	 * The contexts of the window that opened the closed one are put back, as the interpreted executors' are.
	 *
	 * @param close the step being skipped
	 * @throws org.skyve.metadata.MetaDataException	if no {@code <navigateLink window="new"/>} opened a window
	 */
	@Override
	public void executeCloseWindow(@Nonnull CloseWindow close) {
		super.executeCloseWindow(close);
		comment("<closeWindow/> not supported by this generator");
	}

	/**
	 * Emits a comment that {@code <listGridTool/>} is not supported, because this generator has no Selenese equivalent yet.
	 * The context is left as it is.
	 *
	 * @param tool the step being skipped
	 */
	@Override
	public void executeListGridTool(@Nonnull ListGridTool tool) {
		comment("<listGridTool/> not supported by this generator");
	}
}
