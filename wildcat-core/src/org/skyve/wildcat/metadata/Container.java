package org.skyve.wildcat.metadata;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.wildcat.metadata.view.Inject;
import org.skyve.wildcat.metadata.view.container.HBox;
import org.skyve.wildcat.metadata.view.container.TabPane;
import org.skyve.wildcat.metadata.view.container.VBox;
import org.skyve.wildcat.metadata.view.container.form.Form;
import org.skyve.wildcat.metadata.view.widget.Blurb;
import org.skyve.wildcat.metadata.view.widget.Button;
import org.skyve.wildcat.metadata.view.widget.DialogButton;
import org.skyve.wildcat.metadata.view.widget.DynamicImage;
import org.skyve.wildcat.metadata.view.widget.GeoLocator;
import org.skyve.wildcat.metadata.view.widget.Link;
import org.skyve.wildcat.metadata.view.widget.MapDisplay;
import org.skyve.wildcat.metadata.view.widget.StaticImage;
import org.skyve.wildcat.metadata.view.widget.bound.Label;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.wildcat.metadata.view.widget.bound.input.Comparison;
import org.skyve.wildcat.metadata.view.widget.bound.input.ListMembership;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public abstract class Container implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2633803738970828551L;

	@XmlElementRefs({@XmlElementRef(type = StaticImage.class),
						@XmlElementRef(type = VBox.class),
						@XmlElementRef(type = HBox.class),
						@XmlElementRef(type = Form.class),
						@XmlElementRef(type = TabPane.class),
						@XmlElementRef(type = Button.class),
						@XmlElementRef(type = GeoLocator.class),
						@XmlElementRef(type = MapDisplay.class),
						@XmlElementRef(type = DynamicImage.class),
						@XmlElementRef(type = DialogButton.class),
						@XmlElementRef(type = Label.class),
						@XmlElementRef(type = Blurb.class),
						@XmlElementRef(type = Link.class),
						@XmlElementRef(type = ListMembership.class),
						@XmlElementRef(type = CheckMembership.class),
						@XmlElementRef(type = Comparison.class),
						@XmlElementRef(type = DataGrid.class),
						@XmlElementRef(type = ListGrid.class),
						@XmlElementRef(type = Inject.class)})
	private List<MetaData> contained = new ArrayList<>();

	public List<MetaData> getContained() {
		return contained;
	}
}
