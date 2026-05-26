package org.skyve.metadata.sail.language.step;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Executes an inline Groovy/BeanShell script fragment within the current automation context.
 *
 * <p>The script body is stored as a CDATA value in the SAIL XML, allowing multi-line
 * code without XML escaping. The script executes in the context of the current
 * {@link org.skyve.metadata.sail.execution.Executor} implementation and has access
 * to framework scripting bindings.
 *
 * @see org.skyve.metadata.sail.execution.Executor#executeExecute
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Execute implements Step {

	private String script;

	/**
	 * Returns the script.
	 * @return the result
	 */
	public String getScript() {
		return script;
	}

	/**
	 * Sets the script.
	 * @param script the script
	 */
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setScript(String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeExecute(this);
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
