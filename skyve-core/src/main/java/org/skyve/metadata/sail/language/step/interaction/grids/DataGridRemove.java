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
 * Removes the row at the specified zero-based {@code row} index from the datagrid
 * identified by {@code binding}, detaching that record from the collection.
 *
 * <p>Remove does not permanently delete the underlying record from the database;
 * it only detaches it from the collection. Use
 * {@link org.skyve.metadata.sail.language.step.interaction.actions.Delete} to
 * permanently delete.
 *
 * @see DataGridNew
 * @see DataGridEdit
 * @see org.skyve.metadata.sail.execution.Executor#executeDataGridRemove
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataGridRemove implements Step {

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
		executor.executeDataGridRemove(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding + ".remove";
	}
}
