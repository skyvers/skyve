package org.skyve.wildcat.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public final class PickList extends TabularWidget {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -8447209703103356029L;

	private List<PickListColumn> columns = new ArrayList<>();

	private String pickAssociationBinding;

	@Override
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "columns")
	@XmlElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "column", required = true)
	public List<PickListColumn> getColumns() {
		return columns;
	}

	public String getPickAssociationBinding() {
		return pickAssociationBinding;
	}

	@XmlAttribute
	public void setPickAssociationBinding(String pickAssociationBinding) {
		this.pickAssociationBinding = UtilImpl.processStringValue(pickAssociationBinding);
	}
}
