package org.skyve.impl.tools.test.sail.language.step.interaction.grids;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.execution.Executor;
import org.skyve.impl.tools.test.sail.language.Step;
import org.skyve.impl.util.UtilImpl;

/**
 * Remove a row in a data grid at the given row number
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class DataGridRemove implements Step {
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
		executor.execute(this);
	}
}
