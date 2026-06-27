package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Closes the current inline zoom-in edit view
 * ({@link org.skyve.metadata.controller.ImplicitActionName#ZoomOut}) and returns to
 * the parent datagrid or list grid.
 *
 * @see ZoomIn
 * @see org.skyve.metadata.sail.execution.Executor#executeZoomOut
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ZoomOut extends AbstractAction {

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeZoomOut(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return ImplicitActionName.ZoomOut.toString();
	}
}
