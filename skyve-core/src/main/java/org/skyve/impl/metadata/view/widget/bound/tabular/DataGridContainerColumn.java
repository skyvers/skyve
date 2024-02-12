package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;

import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "containerColumn")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"widgets"})
public class DataGridContainerColumn extends DataGridColumn {
	private static final long serialVersionUID = -26924109323814766L;

	private List<MetaData> widgets = new ArrayList<>();

	@XmlElementRefs({@XmlElementRef(type = Link.class),
						@XmlElementRef(type = ContentImage.class),
						@XmlElementRef(type = StaticImage.class),
						@XmlElementRef(type = DynamicImage.class),
						@XmlElementRef(type = Blurb.class),
						@XmlElementRef(type = Label.class)})
	public List<MetaData> getWidgets() {
		return widgets;
	}
}
