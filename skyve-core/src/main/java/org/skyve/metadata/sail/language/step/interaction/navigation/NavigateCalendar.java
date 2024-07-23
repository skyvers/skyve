package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Navigate to a calendar view.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateCalendar extends NavigateList {
	private String startBinding;
	private String endBinding;

	public String getStartBinding() {
		return startBinding;
	}

	@XmlAttribute(name = "startBinding")
	public void setStartBinding(String startBinding) {
		this.startBinding = UtilImpl.processStringValue(startBinding);
	}

	public String getEndBinding() {
		return endBinding;
	}

	@XmlAttribute(name = "endBinding")
	public void setEndBinding(String endBinding) {
		this.endBinding = UtilImpl.processStringValue(endBinding);
	}

	@Override
	public void execute(Executor executor) {
		executor.executeNavigateCalendar(this);
	}
}
