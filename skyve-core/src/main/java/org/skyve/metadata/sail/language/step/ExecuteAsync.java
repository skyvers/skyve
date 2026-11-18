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
 * Runs the body of an {@code async} JavaScript function in the browser page under test, and waits for it to settle,
 * for example {@code <executeAsync variable="status">const r = await fetch(location.href); return r.status;</executeAsync>}.
 *
 * <p>The body is the element's text, usually in CDATA. It runs as {@link Execute}'s does, with the same {@code sail}
 * argument and no token substitution, but in an {@code async} function, so {@code await} works. The step passes when
 * the promise resolves. It fails when the promise rejects, with the error's message, or when it has not settled within
 * {@code timeoutMillis}, which defaults to the run's {@code sail.timeoutSeconds}. With {@code variable}, the resolved
 * value, as text ({@code String.valueOf}), is kept in that variable for {@code {var:name}}; a {@code null} or
 * {@code undefined} value then fails the step. It runs under every test strategy, as it is not an assertion.
 *
 * <p>An empty body, a {@code variable} that is not a variable name ({@value CaptureStep#VARIABLE_NAME_PATTERN}) or a
 * negative timeout fails the step with a {@link MetaDataException} when it runs, as the XSD cannot express these rules.
 *
 * @see Execute
 * @see org.skyve.metadata.sail.execution.Executor#executeExecuteAsync
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ExecuteAsync implements Step {

	private String script;
	private Integer timeoutMillis;
	private String variable;

	/**
	 * Returns the body of the {@code async} function.
	 * @return the script, or {@code null} if the element has no text
	 */
	public @Nullable String getScript() {
		return script;
	}

	/**
	 * Sets the body of the {@code async} function.
	 * @param script the JavaScript, trimmed; blank means none
	 */
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setScript(@Nullable String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	/**
	 * Returns how long the promise may take to settle.
	 * @return the timeout in milliseconds, or {@code null} for the run's {@code sail.timeoutSeconds}
	 */
	public @Nullable Integer getTimeoutMillis() {
		return timeoutMillis;
	}

	/**
	 * Sets how long the promise may take to settle.
	 * @param timeoutMillis the timeout in milliseconds, or {@code null} for the run's {@code sail.timeoutSeconds}
	 */
	@XmlAttribute(name = "timeoutMillis")
	public void setTimeoutMillis(@Nullable Integer timeoutMillis) {
		this.timeoutMillis = timeoutMillis;
	}

	/**
	 * Returns the name of the variable that receives the resolved value.
	 * @return the variable name, or {@code null} to keep no value
	 */
	public @Nullable String getVariable() {
		return variable;
	}

	/**
	 * Sets the name of the variable that receives the resolved value.
	 * @param variable the variable name, matching {@value CaptureStep#VARIABLE_NAME_PATTERN}, or {@code null} to keep
	 * 					no value
	 */
	@XmlAttribute(name = "variable")
	public void setVariable(@Nullable String variable) {
		this.variable = UtilImpl.processStringValue(variable);
	}

	/**
	 * Checks the body, the timeout and the variable name, then dispatches to
	 * {@link Executor#executeExecuteAsync(ExecuteAsync)}.
	 * @param executor the executor
	 * @throws MetaDataException if the body is empty, the timeout is negative, or the variable is not a variable name
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if (script == null) {
			throw new MetaDataException("<executeAsync /> has no script");
		}
		if ((timeoutMillis != null) && (timeoutMillis.intValue() < 0)) {
			throw new MetaDataException("<executeAsync /> [timeoutMillis] cannot be negative");
		}
		if (variable != null) {
			CaptureStep.validateVariableName("executeAsync", variable);
		}
		executor.executeExecuteAsync(this);
	}

	/**
	 * Returns the element name and the script's first line, truncated to 60 characters.
	 * @param context the context, which is not used
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return Execute.scriptIdentifier("executeAsync", script);
	}
}
