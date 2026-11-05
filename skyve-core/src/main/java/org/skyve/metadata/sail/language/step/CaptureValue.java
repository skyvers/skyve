package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
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
 * Capturing the same variable again replaces its value. Variables last for one automation run.
 *
 * <p>{@code variable} must match {@value #VARIABLE_NAME_PATTERN}. The XSD cannot express this, so
 * {@link #execute(Executor)} checks it.
 *
 * @see TestValue
 * @see org.skyve.metadata.sail.execution.Executor#executeCaptureValue
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class CaptureValue implements Step {
	/**
	 * The regular expression that a variable name must match: a letter, then letters, digits or underscores.
	 */
	public static final String VARIABLE_NAME_PATTERN = "[A-Za-z][A-Za-z0-9_]*";

	private String binding;
	private String variable;

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
	 * Checks the variable name, then dispatches to {@link Executor#executeCaptureValue(CaptureValue)}.
	 * @param executor the executor
	 * @throws MetaDataException if the variable name does not match {@value #VARIABLE_NAME_PATTERN}
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if ((variable == null) || (! variable.matches(VARIABLE_NAME_PATTERN))) {
			throw new MetaDataException("<CaptureValue /> [variable] " + variable + " is not a variable name: use a letter, then letters, digits or underscores");
		}
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
