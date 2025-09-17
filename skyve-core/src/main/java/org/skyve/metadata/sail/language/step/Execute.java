package org.skyve.metadata.sail.language.step;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * A SAIL step that executes an inline script.
 * 
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
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setScript(String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	@Override
	public void execute(Executor executor, ExecutionOptions options) {
		executor.executeExecute(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
