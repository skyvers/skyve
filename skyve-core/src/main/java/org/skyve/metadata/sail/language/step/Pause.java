package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Pauses SAIL script execution for the specified number of milliseconds.
 *
 * <p>This is a fixed delay and a last resort. The executors already wait for the
 * asynchronous work a step starts, and {@code <testGridRows timeoutMillis="..."/>}
 * waits for a condition such as a background job's result. Use a pause only when no
 * observable condition exists, and say why in a comment.
 *
 * @see org.skyve.metadata.sail.execution.Executor#executePause
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Pause implements Step {

	private long millis;

	/**
	 * Returns the millis.
	 * @return the result
	 */
	public long getMillis() {
		return millis;
	}

	/**
	 * Sets the millis.
	 * @param millis the millis
	 */
	@XmlAttribute(required = true)
	public void setMillis(long millis) {
		this.millis = millis;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executePause(this);
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
