package org.skyve.metadata.sail.language.step;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Executes an inline JavaScript fragment in the browser page under test.
 *
 * <p>The script body is stored as a CDATA value in the SAIL XML, allowing multi-line
 * code without XML escaping. Browser-driving executors run it in the current page; the
 * interpreted executors then wait for any requests it starts. Executors that generate
 * test code emit it into the generated test. Prefer a dedicated step where one exists:
 * a script can do things no user could.
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
	public @Nullable String getScript() {
		return script;
	}

	/**
	 * Sets the script.
	 * @param script the script
	 */
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setScript(@Nullable String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeExecute(this);
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
