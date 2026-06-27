package org.skyve.impl.metadata.view.widget.bound.tabular;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated tree-structured list grid that renders hierarchical data
 * from a named query or model.
 *
 * <p>Extends {@link ListGrid} with a {@code rootIdBinding} that identifies
 * the binding whose value selects the root node of the tree.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ListGrid
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"rootIdBinding"})
public class TreeGrid extends ListGrid {
	private static final long serialVersionUID = -94277478938378729L;

	private String rootIdBinding;
	
	public String getRootIdBinding() {
		return rootIdBinding;
	}

	@XmlAttribute(name = "rootIdBinding")
	public void setRootIdBinding(String rootIdBinding) {
		this.rootIdBinding = UtilImpl.processStringValue(rootIdBinding);
	}
}
