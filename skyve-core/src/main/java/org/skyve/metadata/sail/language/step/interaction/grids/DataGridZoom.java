package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Zoom into a row in a data grid at the given row number
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataGridZoom implements Step {
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
		executor.executeDataGridZoom(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding + ".zoom";
	}
}
