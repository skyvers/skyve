package org.skyve.impl.metadata.view.widget.bound.tabular;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.view.widget.bound.tabular.TabularColumn;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"title", "pickBinding", "alignment"})
public final class PickListColumn extends AbstractBound implements TabularColumn {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1117229764721545010L;

	private String title;
	private String pickBinding;
	private HorizontalAlignment alignment;

	@Override
	public String getTitle() {
		return title;
	}

	@Override
	@XmlAttribute(required = true)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	public String getPickBinding() {
		return pickBinding;
	}

	@XmlAttribute(required = true)
	public void setPickBinding(String pickBinding) {
		this.pickBinding = UtilImpl.processStringValue(pickBinding);
	}

	@Override
	public HorizontalAlignment getAlignment() {
		return alignment;
	}

	@Override
	@XmlAttribute(required = true)
	public void setAlignment(HorizontalAlignment alignment) {
		this.alignment = alignment;
	}
}
