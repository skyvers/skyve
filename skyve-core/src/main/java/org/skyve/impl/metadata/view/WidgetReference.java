package org.skyve.impl.metadata.view;

import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class WidgetReference implements SerializableMetaData {
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
