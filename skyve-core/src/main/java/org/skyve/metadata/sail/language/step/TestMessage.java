package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Asserts that a message (a growl or a message in the view's message area) whose text
 * contains {@code contains} is currently shown, for example
 * {@code <testMessage contains="Backup started" type="info"/>}.
 *
 * <p>When {@code type} is set, the matching message must also have that severity.
 * The match is a case-sensitive substring match on the message's displayed text.
 * {@code {run}} in {@code contains} is replaced by the run token.
 *
 * <p>Unlike {@link TestSuccess} and {@link TestFailure}, this step asserts information and
 * warning messages as well as errors.
 *
 * @see TestSuccess
 * @see TestFailure
 * @see org.skyve.metadata.sail.execution.Executor#executeTestMessage
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestMessage implements Step {
	/**
	 * The severity of a message asserted by {@link TestMessage}.
	 */
	@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are SAIL XML values.
	public enum MessageType {
		/** An information message. */
		info,
		/** A warning message. */
		warning,
		/** An error message. */
		error
	}

	private String contains;
	private MessageType type;

	/**
	 * Returns the text that the message must contain.
	 * @return the text
	 */
	public @Nonnull String getContains() {
		return contains;
	}

	/**
	 * Sets the text that the message must contain.
	 * @param contains the text; {@code {run}} is replaced by the run token
	 */
	@XmlAttribute(name = "contains", required = true)
	public void setContains(@Nonnull String contains) {
		this.contains = UtilImpl.processStringValue(contains);
	}

	/**
	 * Returns the severity that the message must have.
	 * @return the severity, or {@code null} to accept a message of any severity
	 */
	public @Nullable MessageType getType() {
		return type;
	}

	/**
	 * Sets the severity that the message must have.
	 * @param type the severity, or {@code null} to accept a message of any severity
	 */
	@XmlAttribute(name = "type")
	public void setType(@Nullable MessageType type) {
		this.type = type;
	}

	/**
	 * Dispatches to {@link Executor#executeTestMessage(TestMessage)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeTestMessage(this);
	}

	/**
	 * Returns {@code "Message"}.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return "Message";
	}
}
