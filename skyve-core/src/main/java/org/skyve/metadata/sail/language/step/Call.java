package org.skyve.metadata.sail.language.step;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Runs a handwritten Java step, a {@link org.skyve.metadata.sail.execution.SailCall}, in the JVM that runs the
 * automation, for what a script cannot do in the page, for example
 * <pre>
 * &lt;call className="sail.kitchenSink.EchoCall" variable="echo"&gt;
 * 	&lt;parameter name="text" value="made {run}" /&gt;
 * &lt;/call&gt;
 * </pre>
 *
 * <p>{@code className} names a class that implements {@code SailCall} and has a public no-argument constructor, as
 * actions and Bizlets are named. A new instance is made for each step, from the thread context class loader. It
 * receives the parameters by name, with {@code {run}} and {@code {var:name}} substituted in their values. With
 * {@code variable}, the string it returns is kept in that variable for {@code {var:name}}; a {@code null} return then
 * fails the step.
 *
 * <p>The step fails when the class is missing, is not a {@code SailCall} or cannot be constructed, or when the call
 * throws. Under the {@code Verify} test strategy, an {@link AssertionError} from the call is recorded and the run
 * carries on, as a failed assertion is, and under {@code None} it is ignored. Any other exception, Selenide's
 * {@code UIAssertionError} included, is a step that throws.
 *
 * <p>A missing {@code className}, a parameter without a name or value (a blank one counts as absent, as for other
 * attributes), a parameter name given twice, or a
 * {@code variable} that is not a variable name ({@value CaptureStep#VARIABLE_NAME_PATTERN}) fails the step with a
 * {@link MetaDataException} when it runs, as the XSD cannot express these rules.
 *
 * <p>Not thread-safe: built, or unmarshalled, before the automation runs.
 *
 * @see org.skyve.metadata.sail.execution.SailCall
 * @see org.skyve.metadata.sail.execution.Executor#executeCall
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"className", "variable", "parameters"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Call implements Step {

	private String className;
	private String variable;
	private final List<CallParameter> parameters = new ArrayList<>();

	/**
	 * Returns the name of the class that implements the call.
	 * @return the fully qualified class name; {@code null} only on a step built without one, which
	 * 			{@link #execute(Executor)} rejects
	 */
	public @Nonnull String getClassName() {
		return className;
	}

	/**
	 * Sets the name of the class that implements the call.
	 * @param className the fully qualified name of a {@link org.skyve.metadata.sail.execution.SailCall} class
	 */
	@XmlAttribute(name = "className", required = true)
	public void setClassName(@Nonnull String className) {
		this.className = UtilImpl.processStringValue(className);
	}

	/**
	 * Returns the name of the variable that receives the call's result.
	 * @return the variable name, or {@code null} to keep no result
	 */
	public @Nullable String getVariable() {
		return variable;
	}

	/**
	 * Sets the name of the variable that receives the call's result.
	 * @param variable the variable name, matching {@value CaptureStep#VARIABLE_NAME_PATTERN}, or {@code null} to keep
	 * 					no result
	 */
	@XmlAttribute(name = "variable")
	public void setVariable(@Nullable String variable) {
		this.variable = UtilImpl.processStringValue(variable);
	}

	/**
	 * Returns the call's parameters.
	 * @return the parameters, in the order written (a live, possibly empty list)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "parameter")
	public @Nonnull List<CallParameter> getParameters() {
		return parameters;
	}

	/**
	 * Checks the class name, the parameters and the variable name, then dispatches to
	 * {@link Executor#executeCall(Call)}.
	 *
	 * <p>Complexity: O(p) in the number of parameters.
	 *
	 * @param executor the executor
	 * @throws MetaDataException if the class name is missing, a parameter has no name or value, a name is given twice,
	 * 								or the variable is not a variable name
	 */
	@Override
	// The class name and a parameter's name and value are null on a step built without them (their getters are
	// @Nonnull for a validated step): these are the checks
	@SuppressWarnings({"null", "java:S2589"})
	public void execute(@Nonnull Executor executor) {
		if (className == null) {
			throw new MetaDataException("<call /> has no [className]");
		}
		Set<String> names = new HashSet<>(parameters.size() * 4 / 3 + 1);
		for (CallParameter parameter : parameters) {
			String name = parameter.getName();
			if ((name == null) || (parameter.getValue() == null)) {
				throw new MetaDataException("<call className=\"" + className + "\" /> has a <parameter /> without a [name] or [value]: a blank one counts as absent");
			}
			if (! names.add(name)) {
				throw new MetaDataException("<call className=\"" + className + "\" /> has more than one <parameter name=\"" + name + "\" />");
			}
		}
		if ((variable != null) && (! variable.matches(CaptureStep.VARIABLE_NAME_PATTERN))) {
			throw new MetaDataException("<call /> [variable] " + variable + " is not a variable name: use a letter, then letters, digits or underscores");
		}
		executor.executeCall(this);
	}

	/**
	 * Returns the element name and the class name.
	 * @param context the context, which is not used
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return "call " + className;
	}
}
