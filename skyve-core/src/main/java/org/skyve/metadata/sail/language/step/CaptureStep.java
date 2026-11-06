package org.skyve.metadata.sail.language.step;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlTransient;

/**
 * A step that reads text the page shows and keeps it in a variable, which later steps use as {@code {var:name}}:
 * {@link CaptureValue} (a widget), {@link CaptureCell} (a grid cell) and {@link CaptureTitle} (the view's title).
 *
 * <p>Invariants, checked by {@link #validateCapture(CaptureStep)} from each implementation's
 * {@link Step#execute(org.skyve.metadata.sail.execution.Executor)} because the XSD cannot express them:
 * <ul>
 *   <li>the variable name matches {@value #VARIABLE_NAME_PATTERN};</li>
 *   <li>the {@code pattern}, when given, is a valid Java regular expression.</li>
 * </ul>
 *
 * <p>The text captured is the displayed text, trimmed. With a {@code pattern}, the captured value is the first group of
 * the pattern's first match in the text, or the whole match when the pattern has no group; see
 * {@link #extract(String, String)}. Executors fail the step when the text is blank or the pattern does not match, as a
 * blank variable would match anything.
 *
 * @see org.skyve.metadata.sail.execution.Executor#executeCaptureValue
 * @see org.skyve.metadata.sail.execution.Executor#executeCaptureCell
 * @see org.skyve.metadata.sail.execution.Executor#executeCaptureTitle
 */
@XmlTransient
public interface CaptureStep extends Step {
	/**
	 * The regular expression that a variable name must match: a letter, then letters, digits or underscores.
	 */
	public static final String VARIABLE_NAME_PATTERN = "[A-Za-z][A-Za-z0-9_]*";

	/**
	 * Returns the name of the variable that receives the value.
	 *
	 * @return the variable name; {@code null} only on a step that has not been validated
	 */
	public @Nonnull String getVariable();

	/**
	 * Returns the regular expression that picks the value out of the text read.
	 *
	 * @return the pattern, or {@code null} to keep the whole text
	 */
	public @Nullable String getPattern();

	/**
	 * Checks that a capture step names a valid variable and, if it has one, a valid pattern.
	 *
	 * @param step	the step to check
	 * @throws MetaDataException	if the variable name does not match {@value #VARIABLE_NAME_PATTERN}, or the pattern is not
	 * 								a valid regular expression
	 */
	@SuppressWarnings("null") // the variable is null on a step read from XML without it
	public static void validateCapture(@Nonnull CaptureStep step) {
		String variable = step.getVariable();
		String element = step.getClass().getSimpleName();
		if ((variable == null) || (! variable.matches(VARIABLE_NAME_PATTERN))) {
			throw new MetaDataException("<" + element + " /> [variable] " + variable + " is not a variable name: use a letter, then letters, digits or underscores");
		}
		String pattern = step.getPattern();
		if (pattern != null) {
			try {
				Pattern.compile(pattern);
			}
			catch (PatternSyntaxException e) {
				throw new MetaDataException("<" + element + " /> [pattern] " + pattern + " is not a regular expression: " + e.getDescription(), e);
			}
		}
	}

	/**
	 * Picks the value out of the text read: the first group of the pattern's first match, or the whole match when the
	 * pattern has no group, or the text itself when there is no pattern.
	 *
	 * <p>Complexity: that of {@link Matcher#find()} over {@code text}.
	 *
	 * @param pattern	the regular expression, or {@code null} to keep the whole text
	 * @param text	the text read
	 * @return the value, or {@code null} if the pattern does not match, or its first group took no part in the match
	 */
	public static @Nullable String extract(@Nullable String pattern, @Nonnull String text) {
		if (pattern == null) {
			return text;
		}
		Matcher matcher = Pattern.compile(pattern).matcher(text);
		if (! matcher.find()) {
			return null;
		}
		return (matcher.groupCount() > 0) ? matcher.group(1) : matcher.group();
	}
}
