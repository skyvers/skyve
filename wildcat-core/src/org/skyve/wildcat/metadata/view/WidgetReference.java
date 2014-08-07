package org.skyve.wildcat.metadata.view;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckBox;
import org.skyve.wildcat.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.wildcat.metadata.view.widget.bound.input.Combo;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentImage;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentLink;
import org.skyve.wildcat.metadata.view.widget.bound.input.Geometry;
import org.skyve.wildcat.metadata.view.widget.bound.input.HTML;
import org.skyve.wildcat.metadata.view.widget.bound.input.InputWidget;
import org.skyve.wildcat.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.wildcat.metadata.view.widget.bound.input.Password;
import org.skyve.wildcat.metadata.view.widget.bound.input.Radio;
import org.skyve.wildcat.metadata.view.widget.bound.input.RichText;
import org.skyve.wildcat.metadata.view.widget.bound.input.Slider;
import org.skyve.wildcat.metadata.view.widget.bound.input.Spinner;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextArea;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextField;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public class WidgetReference implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4623822506842372448L;

	private InputWidget widget;
	
	public InputWidget getWidget() {
		return widget;
	}

	@XmlElementRefs({@XmlElementRef(type = ContentImage.class),
						@XmlElementRef(type = ContentLink.class), 
						@XmlElementRef(type = CheckBox.class),
						@XmlElementRef(type = ColourPicker.class), 
						@XmlElementRef(type = Combo.class), 
						@XmlElementRef(type = Geometry.class),
						@XmlElementRef(type = HTML.class),
						@XmlElementRef(type = LookupDescription.class),
						@XmlElementRef(type = Password.class), 
						@XmlElementRef(type = Radio.class),
						@XmlElementRef(type = RichText.class), 
						@XmlElementRef(type = Slider.class),
						@XmlElementRef(type = Spinner.class),
						@XmlElementRef(type = TextArea.class),
						@XmlElementRef(type = TextField.class)})
	public void setWidget(InputWidget widget) {
		this.widget = widget;
	}
}
