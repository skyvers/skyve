package org.skyve.metadata.sail.language.step.interaction.session;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

/**
 * Logout.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Logout implements Step {
	@Override
	public void execute(Executor executor) {
		executor.executeLogout(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
