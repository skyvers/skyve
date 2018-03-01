package org.skyve.impl.tools.test.sail.language.step;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.execution.Executor;
import org.skyve.impl.tools.test.sail.language.Step;
import org.skyve.impl.util.UtilImpl;

/**
 * Executes some inline script.
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
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
		executor.execute(this);
	}
}
