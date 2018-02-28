package org.skyve.impl.tools.test.sail.language.step.interaction.lookup;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

/**
 * Auto-complete by entering the search string and selecting the first match from the drop down.
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class LookupDescriptionAutoComplete implements Step {
	private String binding;
	private String search;
	
	public String getBinding() {
		return binding;
	}

	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = binding;
	}

	public String getSearch() {
		return search;
	}

	@XmlAttribute(name = "search", required = true)
	public void setSearch(String search) {
		this.search = search;
	}

	@Override
	public void execute(StringBuilder script, int indentationDepth) {
	}
}
