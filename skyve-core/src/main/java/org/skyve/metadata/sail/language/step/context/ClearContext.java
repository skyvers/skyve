package org.skyve.metadata.sail.language.step.context;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

/**
 * Clear all automation contexts from the stack.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ClearContext implements Step {
	@Override
	public void execute(Executor executor) {
		executor.executeClearContext(this);
	}

	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
