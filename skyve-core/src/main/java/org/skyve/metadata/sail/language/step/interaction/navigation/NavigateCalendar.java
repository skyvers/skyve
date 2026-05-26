package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Navigates to the calendar view for the module and document specified by the
 * inherited {@link NavigateList} attributes, optionally configuring the date
 * range bindings.
 *
 * <p>{@code startBinding} and {@code endBinding} identify the document attributes
 * that supply the start and end dates for each calendar entry.
 *
 * @see NavigateList
 * @see NavigateMap
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateCalendar
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateCalendar extends NavigateList {
	private String startBinding;
	private String endBinding;

	/**
	 * Returns the startBinding.
	 * @return the result
	 */
	public String getStartBinding() {
		return startBinding;
	}

	/**
	 * Sets the startBinding.
	 * @param startBinding the startBinding
	 */
	@XmlAttribute(name = "startBinding")
	public void setStartBinding(String startBinding) {
		this.startBinding = UtilImpl.processStringValue(startBinding);
	}

	/**
	 * Returns the endBinding.
	 * @return the result
	 */
	public String getEndBinding() {
		return endBinding;
	}

	/**
	 * Sets the endBinding.
	 * @param endBinding the endBinding
	 */
	@XmlAttribute(name = "endBinding")
	public void setEndBinding(String endBinding) {
		this.endBinding = UtilImpl.processStringValue(endBinding);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeNavigateCalendar(this);
	}
}
