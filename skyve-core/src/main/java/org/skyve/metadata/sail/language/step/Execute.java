package org.skyve.metadata.sail.language.step;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Executes an inline JavaScript fragment in the browser page under test.
 *
 * <p>The script body is stored as a CDATA value in the SAIL XML, allowing multi-line
 * code without XML escaping. Browser-driving executors run it in the current page; the
 * interpreted executors then wait for any requests it starts. Executors that generate
 * test code emit it into the generated test. Prefer a dedicated step where one exists:
 * a script can do things no user could.
 *
 * <p>The interpreted executors run the body as a function of one argument, {@code sail}, which is also
 * {@code arguments[0]}: {@code {run: "<run token>", vars: {name: "value", ...}}}, a copy of the run token and the
 * variables. Changing it changes nothing in the run. The body is never token-substituted: its braces are JavaScript.
 * {@code return} gives the step's result. With {@code variable}, the result, as text ({@code String.valueOf}), is kept
 * in that variable for {@code {var:name}}. The step fails when the script throws, or when {@code variable} is set and
 * the result is {@code null} or {@code undefined}. It runs under every test strategy, as it is not an assertion.
 *
 * <p>An empty body, or a {@code variable} that is not a variable name ({@value CaptureStep#VARIABLE_NAME_PATTERN}),
 * fails the step with a {@link MetaDataException} when it runs, as the XSD cannot express these rules.
 *
 * @see ExecuteAsync
 * @see org.skyve.metadata.sail.execution.Executor#executeExecute
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Execute implements Step {

	private String script;
	private String variable;

	/**
	 * Returns the script.
	 * @return the result
	 */
	public @Nullable String getScript() {
		return script;
	}

	/**
	 * Sets the script.
	 * @param script the script
	 */
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setScript(@Nullable String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	/**
	 * Returns the name of the variable that receives the script's result.
	 * @return the variable name, or {@code null} to keep no result
	 */
	public @Nullable String getVariable() {
		return variable;
	}

	/**
	 * Sets the name of the variable that receives the script's result.
	 * @param variable the variable name, matching {@value CaptureStep#VARIABLE_NAME_PATTERN}, or {@code null} to keep
	 * 					no result
	 */
	@XmlAttribute(name = "variable")
	public void setVariable(@Nullable String variable) {
		this.variable = UtilImpl.processStringValue(variable);
	}

	/**
	 * Checks the body and the variable name, then dispatches to {@link Executor#executeExecute(Execute)}.
	 * @param executor the executor
	 * @throws MetaDataException if the body is empty, or the variable is not a variable name
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if (script == null) {
			throw new MetaDataException("<execute /> has no script");
		}
		if ((variable != null) && (! variable.matches(CaptureStep.VARIABLE_NAME_PATTERN))) {
			throw new MetaDataException("<execute /> [variable] " + variable + " is not a variable name: use a letter, then letters, digits or underscores");
		}
		executor.executeExecute(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nullable String getIdentifier(@Nonnull AutomationContext context) {
		return null;
	}
}
