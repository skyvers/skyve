package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Reads the title that the current view shows and keeps it in the named variable, for example
 * {@code <captureTitle variable="request" pattern="Request (\d+)"/>}. Later steps use it as {@code {var:request}}.
 *
 * <p>This is for values that a view shows only in its title, such as a record's reference number in a title made from
 * its bizKey. The value is kept as {@link CaptureValue} keeps it: the displayed text, trimmed, optionally picked out by
 * {@code pattern}; a blank title, or a pattern that does not match, fails the step.
 *
 * <p>The variable name and pattern are checked by {@link #execute(Executor)}, as the XSD cannot express them.
 *
 * @see CaptureValue
 * @see org.skyve.metadata.sail.execution.Executor#executeCaptureTitle
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class CaptureTitle implements CaptureStep {
	private String variable;
	private String pattern;

	/**
	 * Returns the name of the variable that receives the value.
	 * @return the variable name
	 */
	@Override
	public @Nonnull String getVariable() {
		return variable;
	}

	/**
	 * Sets the name of the variable that receives the value.
	 * @param variable the variable name, matching {@value CaptureStep#VARIABLE_NAME_PATTERN}
	 */
	@XmlAttribute(name = "variable", required = true)
	public void setVariable(@Nonnull String variable) {
		this.variable = UtilImpl.processStringValue(variable);
	}

	/**
	 * Returns the regular expression that picks the value out of the title.
	 * @return the pattern, or {@code null} to keep the whole title
	 */
	@Override
	public @Nullable String getPattern() {
		return pattern;
	}

	/**
	 * Sets the regular expression that picks the value out of the title: its first group, or the whole match when it
	 * has no group.
	 * @param pattern the Java regular expression, or {@code null} to keep the whole title
	 */
	@XmlAttribute(name = "pattern")
	public void setPattern(@Nullable String pattern) {
		this.pattern = UtilImpl.processStringValue(pattern);
	}

	/**
	 * Checks the variable name and pattern, then dispatches to {@link Executor#executeCaptureTitle(CaptureTitle)}.
	 * @param executor the executor
	 * @throws org.skyve.metadata.MetaDataException if the variable name or the pattern is invalid
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		CaptureStep.validateCapture(this);
		executor.executeCaptureTitle(this);
	}

	/**
	 * Returns the variable name, as the step reads no widget.
	 * @param context the context
	 * @return the variable name
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return variable;
	}
}
