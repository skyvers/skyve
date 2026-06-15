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
 * Selects the row at the specified zero-based {@code row} index in the datagrid
 * identified by {@code binding} without opening an inline editor or zooming.
 *
 * <p>Use this step when a subsequent action requires a row to be highlighted/selected
 * first, but no data entry on that row is needed.
 *
 * @see DataGridEdit
 * @see DataGridZoom
 * @see org.skyve.metadata.sail.execution.Executor#executeDataGridSelect
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataGridSelect implements Step {

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
		executor.executeDataGridSelect(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding + ".select";
	}
}
