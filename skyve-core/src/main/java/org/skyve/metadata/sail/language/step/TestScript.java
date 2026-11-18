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
 * Asserts, in JavaScript, something about the browser page under test, for example
 * {@code <testScript message="The call should echo">return sail.vars.echo === 'made ' + sail.run;</testScript>}.
 *
 * <p>The body is the element's text, usually in CDATA. It runs as {@link ExecuteAsync}'s does, with the same
 * {@code sail} argument and no token substitution. The assertion holds when the script returns (or resolves to) a
 * truthy value. Without {@code timeoutMillis} the script runs once; with it, the script runs every 250 ms until it
 * holds or the timeout passes. A run that throws or rejects counts as false. The failure's message is {@code message},
 * with {@code {run}} and {@code {var:name}} substituted, or the script's first line.
 *
 * <p>This is an assertion, so it follows the test strategy: under {@code Verify} a failure is recorded and the run
 * carries on, and under {@code None} the step is skipped.
 *
 * <p>An empty body or a negative timeout fails the step with a {@link MetaDataException} when it runs, as the XSD
 * cannot express these rules.
 *
 * @see WaitFor
 * @see org.skyve.metadata.sail.execution.Executor#executeTestScript
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestScript implements Step {

	private String script;
	private Integer timeoutMillis;
	private String message;

	/**
	 * Returns the assertion: the body of an {@code async} function.
	 * @return the script, or {@code null} if the element has no text
	 */
	public @Nullable String getScript() {
		return script;
	}

	/**
	 * Sets the assertion: the body of an {@code async} function.
	 * @param script the JavaScript, trimmed; blank means none
	 */
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setScript(@Nullable String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	/**
	 * Returns how long to keep running the script until it holds.
	 * @return the timeout in milliseconds, or {@code null} to run it once
	 */
	public @Nullable Integer getTimeoutMillis() {
		return timeoutMillis;
	}

	/**
	 * Sets how long to keep running the script until it holds.
	 * @param timeoutMillis the timeout in milliseconds, or {@code null} to run it once
	 */
	@XmlAttribute(name = "timeoutMillis")
	public void setTimeoutMillis(@Nullable Integer timeoutMillis) {
		this.timeoutMillis = timeoutMillis;
	}

	/**
	 * Returns the failure's message.
	 * @return the message, before token substitution, or {@code null} to use the script's first line
	 */
	public @Nullable String getMessage() {
		return message;
	}

	/**
	 * Sets the failure's message.
	 * @param message the message, in which {@code {run}} and {@code {var:name}} are substituted, or {@code null} to
	 * 					use the script's first line
	 */
	@XmlAttribute(name = "message")
	public void setMessage(@Nullable String message) {
		this.message = UtilImpl.processStringValue(message);
	}

	/**
	 * Checks the body and the timeout, then dispatches to {@link Executor#executeTestScript(TestScript)}.
	 * @param executor the executor
	 * @throws MetaDataException if the body is empty or the timeout is negative
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if (script == null) {
			throw new MetaDataException("<testScript /> has no script");
		}
		if ((timeoutMillis != null) && (timeoutMillis.intValue() < 0)) {
			throw new MetaDataException("<testScript /> [timeoutMillis] cannot be negative");
		}
		executor.executeTestScript(this);
	}

	/**
	 * Returns the element name and the script's first line, truncated to 60 characters.
	 * @param context the context, which is not used
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		String line = (script == null) ? "" : script.split("\\R", 2)[0];
		return "testScript " + ((line.length() > 60) ? line.substring(0, 60) : line);
	}
}
