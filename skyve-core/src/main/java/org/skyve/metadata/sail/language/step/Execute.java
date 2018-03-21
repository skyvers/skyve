package org.skyve.metadata.sail.language.step;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Executes some inline script.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Execute implements Step {
	private String script;

	public String getScript() {
		return script;
	}

	@XmlValue
	public void setScript(String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	@Override
	public void execute(Executor executor) {
		executor.executeExecute(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
