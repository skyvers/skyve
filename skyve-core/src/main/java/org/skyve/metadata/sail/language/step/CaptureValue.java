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
 * Reads the value that the widget bound to {@code binding} shows and keeps it in the named variable,
 * for example {@code <captureValue binding="invoiceNumber" variable="invoice"/>}. Later steps use it as
 * {@code {var:invoice}} wherever {@code {run}} is substituted.
 *
 * <p>This is for values the application made, such as a generated number or a bizKey, that a later step
 * must find again. The value captured is the displayed text, trimmed, as {@link TestValue} compares it.
 * Capturing a widget that shows no value fails the step, as a blank variable would match anything.
 * Capturing the same variable again replaces its value. Variables last for one automation run, or for a whole
 * workflow when the automations share their run.
 *
 * <p>With a {@code pattern}, only the part of the text that the regular expression picks is kept (its first group, or
 * the whole match); see {@link CaptureStep#extract(String, String)}. For example
 * {@code <captureValue binding="reference" variable="number" pattern="REF-(\d+)"/>} keeps the digits.
 *
 * <p>{@code variable} must match {@value #VARIABLE_NAME_PATTERN} and {@code pattern} must be a regular expression.
 * The XSD cannot express this, so {@link #execute(Executor)} checks it.
 *
 * @see TestValue
 * @see CaptureCell
 * @see CaptureTitle
 * @see org.skyve.metadata.sail.execution.Executor#executeCaptureValue
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class CaptureValue implements CaptureStep {
	private String binding;
	private String variable;
	private String pattern;

	/**
	 * Returns the binding of the widget to read.
	 * @return the binding
	 */
	public @Nonnull String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding of the widget to read.
	 * @param binding the binding
	 */
	@XmlAttribute(name = "binding", required = true)
	public void setBinding(@Nonnull String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

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
	 * @param variable the variable name, matching {@value #VARIABLE_NAME_PATTERN}
	 */
	@XmlAttribute(name = "variable", required = true)
	public void setVariable(@Nonnull String variable) {
		this.variable = UtilImpl.processStringValue(variable);
	}

	/**
	 * Returns the regular expression that picks the value out of the text the widget shows.
	 * @return the pattern, or {@code null} to keep the whole text
	 */
	@Override
	public @Nullable String getPattern() {
		return pattern;
	}

	/**
	 * Sets the regular expression that picks the value out of the text the widget shows: its first group, or the whole
	 * match when it has no group. The text is not trimmed of what the pattern leaves out.
	 * @param pattern the Java regular expression, or {@code null} to keep the whole text
	 */
	@XmlAttribute(name = "pattern")
	public void setPattern(@Nullable String pattern) {
		this.pattern = UtilImpl.processStringValue(pattern);
	}

	/**
	 * Checks the variable name and pattern, then dispatches to {@link Executor#executeCaptureValue(CaptureValue)}.
	 * @param executor the executor
	 * @throws org.skyve.metadata.MetaDataException if the variable name does not match {@value #VARIABLE_NAME_PATTERN}, or the pattern is not
	 * 								a regular expression
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		CaptureStep.validateCapture(this);
		executor.executeCaptureValue(this);
	}

	/**
	 * Returns the binding.
	 * @param context the context
	 * @return the binding
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return binding;
	}
}
