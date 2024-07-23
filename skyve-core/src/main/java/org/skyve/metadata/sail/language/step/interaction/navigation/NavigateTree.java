package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Navigate to a tree view.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateTree extends NavigateList {
	@Override
	public void execute(Executor executor) {
		executor.executeNavigateTree(this);
	}
}
