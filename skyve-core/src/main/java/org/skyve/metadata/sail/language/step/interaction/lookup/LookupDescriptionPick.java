package org.skyve.metadata.sail.language.step.interaction.lookup;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Pick the object represented in the drop down at the given row
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class LookupDescriptionPick implements Step {
	private String binding;
	private Integer row;
	
	public String getBinding() {
		return binding;
	}

	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	public Integer getRow() {
		return row;
	}

	@XmlAttribute(name = "row", required = true)
	public void setRow(Integer row) {
		this.row = row;
	}

	@Override
	public void execute(Executor executor) {
		executor.executeLookupDescriptionPick(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding + ".pick";
	}
}
