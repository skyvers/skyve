package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Opens the inline editor for the row at the specified zero-based {@code row} index
 * within the datagrid identified by {@code binding}.
 *
 * <p>The executor activates the row's inline editing mode, making its cells
 * editable, so that subsequent {@link org.skyve.metadata.sail.language.step.interaction.DataEnter}
 * steps can set values.
 *
 * @see DataGridNew
 * @see DataGridSelect
 * @see DataGridZoom
 * @see org.skyve.metadata.sail.execution.Executor#executeDataGridEdit
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataGridEdit implements Step {

	private String binding;
	private Integer row;
	
	/**
	 * Returns the binding.
	 * @return the result
	 */
	public String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding.
	 * @param binding the binding
	 */
	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Returns the row.
	 * @return the result
	 */
	public Integer getRow() {
		return row;
	}

	/**
	 * Sets the row.
	 * @param row the row
	 */
	@XmlAttribute(name = "row", required = true)
	public void setRow(Integer row) {
		this.row = row;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeDataGridEdit(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding + ".edit";
	}
}
