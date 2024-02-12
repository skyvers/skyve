package org.skyve.impl.metadata.view.widget.bound.tabular;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

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
