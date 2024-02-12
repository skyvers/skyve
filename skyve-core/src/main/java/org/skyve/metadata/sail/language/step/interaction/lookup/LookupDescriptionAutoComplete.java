package org.skyve.metadata.sail.language.step.interaction.lookup;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Auto-complete by entering the search string and selecting the first match from the drop down.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class LookupDescriptionAutoComplete implements Step {
	private String binding;
	private String search;
	
	public String getBinding() {
		return binding;
	}

	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	public String getSearch() {
		return search;
	}

	@XmlAttribute(name = "search", required = true)
	public void setSearch(String search) {
		this.search = UtilImpl.processStringValue(search);
	}

	@Override
	public void execute(Executor executor) {
		executor.executeLookupDescriptionAutoComplete(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding + ".search";
	}
}
