package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Pauses for a number of millis.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Pause implements Step {
	private long millis;

	public long getMillis() {
		return millis;
	}

	@XmlAttribute(required = true)
	public void setMillis(long millis) {
		this.millis = millis;
	}

	@Override
	public void execute(Executor executor) {
		executor.executePause(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
