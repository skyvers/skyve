package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Navigates to the tree view for the module and document specified by the
 * inherited {@link NavigateList} attributes.
 *
 * @see NavigateList
 * @see NavigateMap
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateTree
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateTree extends NavigateList {
	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeNavigateTree(this);
	}
}
