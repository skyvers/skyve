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
 * Waits until a JavaScript condition in the browser page under test holds, for example
 * {@code <waitFor timeoutMillis="20000">return document.readyState === 'complete';</waitFor>}.
 *
 * <p>The body is the element's text, usually in CDATA. It runs as {@link ExecuteAsync}'s does, with the same
 * {@code sail} argument and no token substitution, every 250 ms until it returns (or resolves to) a truthy value. A
 * poll that throws or rejects counts as false. The step fails when {@code timeoutMillis}, which defaults to the run's
 * {@code sail.timeoutSeconds}, passes first; the message gives the timeout, the script's first line and the last
 * error a poll threw. This is a wait, not a test: it runs under every test strategy, as a timed
 * {@code <testGridRows/>} does, and a timeout is a step that throws, not a failed assertion.
 *
 * <p>An empty body or a negative timeout fails the step with a {@link MetaDataException} when it runs, as the XSD
 * cannot express these rules.
 *
 * @see TestScript
 * @see org.skyve.metadata.sail.execution.Executor#executeWaitFor
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class WaitFor implements Step {

	private String script;
	private Integer timeoutMillis;

	/**
	 * Returns the condition: the body of an {@code async} function.
	 * @return the script, or {@code null} if the element has no text
	 */
	public @Nullable String getScript() {
		return script;
	}

	/**
	 * Sets the condition: the body of an {@code async} function.
	 * @param script the JavaScript, trimmed; blank means none
	 */
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setScript(@Nullable String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	/**
	 * Returns how long to wait for the condition to hold.
	 * @return the timeout in milliseconds, or {@code null} for the run's {@code sail.timeoutSeconds}
	 */
	public @Nullable Integer getTimeoutMillis() {
		return timeoutMillis;
	}

	/**
	 * Sets how long to wait for the condition to hold.
	 * @param timeoutMillis the timeout in milliseconds, or {@code null} for the run's {@code sail.timeoutSeconds}
	 */
	@XmlAttribute(name = "timeoutMillis")
	public void setTimeoutMillis(@Nullable Integer timeoutMillis) {
		this.timeoutMillis = timeoutMillis;
	}

	/**
	 * Checks the body and the timeout, then dispatches to {@link Executor#executeWaitFor(WaitFor)}.
	 * @param executor the executor
	 * @throws MetaDataException if the body is empty or the timeout is negative
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if (script == null) {
			throw new MetaDataException("<waitFor /> has no script");
		}
		if ((timeoutMillis != null) && (timeoutMillis.intValue() < 0)) {
			throw new MetaDataException("<waitFor /> [timeoutMillis] cannot be negative");
		}
		executor.executeWaitFor(this);
	}

	/**
	 * Returns the element name and the script's first line, truncated to 60 characters.
	 * @param context the context, which is not used
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		String line = (script == null) ? "" : script.split("\\R", 2)[0];
		return "waitFor " + ((line.length() > 60) ? line.substring(0, 60) : line);
	}
}
