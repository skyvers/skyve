package org.skyve.impl.metadata;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public abstract class Container implements SerializableMetaData {
	private static final long serialVersionUID = 2633803738970828551L;

	@XmlElementRefs({@XmlElementRef(type = StaticImage.class),
						@XmlElementRef(type = VBox.class),
						@XmlElementRef(type = HBox.class),
						@XmlElementRef(type = Form.class),
						@XmlElementRef(type = TabPane.class),
						@XmlElementRef(type = Button.class),
						@XmlElementRef(type = ZoomIn.class),
						@XmlElementRef(type = Chart.class),
						@XmlElementRef(type = MapDisplay.class),
						@XmlElementRef(type = DynamicImage.class),
						@XmlElementRef(type = DialogButton.class),
						@XmlElementRef(type = Label.class),
						@XmlElementRef(type = Blurb.class),
						@XmlElementRef(type = Link.class),
						@XmlElementRef(type = Spacer.class),
						@XmlElementRef(type = ListMembership.class),
						@XmlElementRef(type = CheckMembership.class),
						@XmlElementRef(type = Comparison.class),
						@XmlElementRef(type = DataGrid.class),
						@XmlElementRef(type = ListGrid.class),
						@XmlElementRef(type = TreeGrid.class),
						@XmlElementRef(type = DataRepeater.class),
						@XmlElementRef(type = ListRepeater.class),
						@XmlElementRef(type = Inject.class),
						@XmlElementRef(type = Component.class)})
	private List<MetaData> contained = new ArrayList<>();

	public List<MetaData> getContained() {
		return contained;
	}
}
