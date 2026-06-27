package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Pauses SAIL script execution for the specified number of milliseconds.
 *
 * <p>Use this step to introduce a deliberate wait between interactions, for example
 * to allow asynchronous UI updates, animations, or server-side processing to complete
 * before the next assertion or interaction step runs.
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
	public void execute(Executor executor) {
		executor.executePause(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
