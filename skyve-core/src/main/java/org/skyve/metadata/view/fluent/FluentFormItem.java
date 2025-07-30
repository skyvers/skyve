package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.metadata.MetaData;

public class FluentFormItem {
	private FormItem item = null;
	
	public FluentFormItem() {
		item = new FormItem();
	}

	public FluentFormItem(FormItem item) {
		this.item = item;
	}

	public FluentFormItem from(@SuppressWarnings("hiding") FormItem item) {
		Integer i = item.getColspan();
		if (i != null) {
			colspan(i.intValue());
		}
		i = item.getRowspan();
		if (i != null) {
			rowspan(i.intValue());
		}
		horizontalAlignment(item.getHorizontalAlignment());
		label(item.getLabel());
		Boolean b = item.getShowLabel();
		if (b != null) {
			showLabel(b.booleanValue());
		}
		labelHorizontalAlignment(item.getLabelHorizontalAlignment());
		if (b != null) {
			showHelp(b.booleanValue());
		}
		help(item.getHelp());
		b = item.getRequired();
		if (b != null) {
			required(b.booleanValue());
		}

		MetaData widget = item.getWidget();
		if (widget instanceof DefaultWidget defaultWidget) {
			defaultWidget(new FluentDefaultWidget().from(defaultWidget));
		}
		else if (widget instanceof ContentImage content) {
			contentImage(new FluentContentImage().from(content));
		}
		else if (widget instanceof ContentLink content) {
			contentLink(new FluentContentLink().from(content));
		}
		else if (widget instanceof ContentSignature content) {
			contentSignature(new FluentContentSignature().from(content));
		}
		else if (widget instanceof Button button) {
			button(new FluentButton().from(button));
		}
		else if (widget instanceof ZoomIn zoom) {
			zoomIn(new FluentZoomIn().from(zoom));
		}
		else if (widget instanceof DialogButton button) {
			dialogButton(new FluentDialogButton().from(button));
		}
		else if (widget instanceof Geometry geometry) {
			geometry(new FluentGeometry().from(geometry));
		}
		else if (widget instanceof GeometryMap map) {
			geometryMap(new FluentGeometryMap().from(map));
		}
		else if (widget instanceof HTML html) {
			html(new FluentHTML().from(html));
		}
		else if (widget instanceof Label label) {
			label(new FluentLabel().from(label));
		}
		else if (widget instanceof Blurb blurb) {
			blurb(new FluentBlurb().from(blurb));
		}
		else if (widget instanceof ProgressBar progress) {
			progressBar(new FluentProgressBar().from(progress));
		}
		else if (widget instanceof CheckBox check) {
			checkBox(new FluentCheckBox().from(check));
		}
		else if (widget instanceof ColourPicker colour) {
			colourPicker(new FluentColourPicker().from(colour));
		}
		else if (widget instanceof Combo combo) {
			combo(new FluentCombo().from(combo));
		}
		else if (widget instanceof Radio radio) {
			radio(new FluentRadio().from(radio));
		}
		else if (widget instanceof LookupDescription lookup) {
			lookupDescription(new FluentLookupDescription().from(lookup));
		}
		else if (widget instanceof Password password) {
			password(new FluentPassword().from(password));
		}
		else if (widget instanceof RichText text) {
			richText(new FluentRichText().from(text));
		}
		else if (widget instanceof Slider slider) {
			slider(new FluentSlider().from(slider));
		}
		else if (widget instanceof Spacer spacer) {
			spacer(new FluentSpacer().from(spacer));
		}
		else if (widget instanceof Spinner spinner) {
			spinner(new FluentSpinner().from(spinner));
		}
		else if (widget instanceof StaticImage image) {
			staticImage(new FluentStaticImage().from(image));
		}
		else if (widget instanceof Link link) {
			link(new FluentLink().from(link));
		}
		else if (widget instanceof TextArea text) {
			textArea(new FluentTextArea().from(text));
		}
		else if (widget instanceof TextField text) {
			textField(new FluentTextField().from(text));
		}
		else if (widget instanceof Inject inject) {
			inject(new FluentInject().from(inject));
		}
		else {
			throw new IllegalStateException(widget + " is not catered for");
		}

		return this;
	}

	public FluentFormItem colspan(int colspan) {
		item.setColspan(Integer.valueOf(colspan));
		return this;
	}
	
	public FluentFormItem rowspan(int rowspan) {
		item.setRowspan(Integer.valueOf(rowspan));
		return this;
	}

	public FluentFormItem horizontalAlignment(HorizontalAlignment horizontalAlignment) {
		item.setHorizontalAlignment(horizontalAlignment);
		return this;
	}

	public FluentFormItem label(String label) {
		item.setLabel(label);
		return this;
	}

	public FluentFormItem showLabel(boolean showLabel) {
		item.setShowLabel(showLabel ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentFormItem labelHorizontalAlignment(HorizontalAlignment labelHorizontalAlignment) {
		item.setLabelHorizontalAlignment(labelHorizontalAlignment);
		return this;
	}

	public FluentFormItem showHelp(boolean showHelp) {
		item.setShowHelp(showHelp ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentFormItem help(String help) {
		item.setHelp(help);
		return this;
	}
	
	public FluentFormItem required(boolean required) {
		item.setRequired(required ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentFormItem defaultWidget(FluentDefaultWidget widget) {
		item.setWidget(widget.get());
		return this;
	}
	
	public FluentFormItem contentImage(FluentContentImage image) {
		item.setWidget(image.get());
		return this;
	}
	
	public FluentFormItem contentLink(FluentContentLink link) {
		item.setWidget(link.get());
		return this;
	}
	
	public FluentFormItem contentSignature(FluentContentSignature signature) {
		item.setWidget(signature.get());
		return this;
	}
	
	public FluentFormItem button(FluentButton button) {
		item.setWidget(button.get());
		return this;
	}
	
	public FluentFormItem zoomIn(FluentZoomIn zoomIn) {
		item.setWidget(zoomIn.get());
		return this;
	}
	
	public FluentFormItem dialogButton(FluentDialogButton button) {
		item.setWidget(button.get());
		return this;
	}
	
	public FluentFormItem geometry(FluentGeometry geometry) {
		item.setWidget(geometry.get());
		return this;
	}

	public FluentFormItem geometryMap(FluentGeometryMap map) {
		item.setWidget(map.get());
		return this;
	}
	
	public FluentFormItem html(FluentHTML html) {
		item.setWidget(html.get());
		return this;
	}
	
	public FluentFormItem label(FluentLabel label) {
		item.setWidget(label.get());
		return this;
	}
	
	public FluentFormItem blurb(FluentBlurb blurb) {
		item.setWidget(blurb.get());
		return this;
	}
	
	public FluentFormItem progressBar(FluentProgressBar bar) {
		item.setWidget(bar.get());
		return this;
	}
	
	public FluentFormItem checkBox(FluentCheckBox check) {
		item.setWidget(check.get());
		return this;
	}
	
	public FluentFormItem colourPicker(FluentColourPicker colour) {
		item.setWidget(colour.get());
		return this;
	}
	
	public FluentFormItem combo(FluentCombo combo) {
		item.setWidget(combo.get());
		return this;
	}
	
	public FluentFormItem radio(FluentRadio radio) {
		item.setWidget(radio.get());
		return this;
	}
	
	public FluentFormItem lookupDescription(FluentLookupDescription lookup) {
		item.setWidget(lookup.get());
		return this;
	}
	
	public FluentFormItem password(FluentPassword password) {
		item.setWidget(password.get());
		return this;
	}
	
	public FluentFormItem richText(FluentRichText text) {
		item.setWidget(text.get());
		return this;
	}
	
	public FluentFormItem slider(FluentSlider slider) {
		item.setWidget(slider.get());
		return this;
	}
	
	public FluentFormItem spacer(FluentSpacer spacer) {
		item.setWidget(spacer.get());
		return this;
	}
	
	public FluentFormItem spinner(FluentSpinner spinner) {
		item.setWidget(spinner.get());
		return this;
	}
	
	public FluentFormItem staticImage(FluentStaticImage image) {
		item.setWidget(image.get());
		return this;
	}

	public FluentFormItem link(FluentLink link) {
		item.setWidget(link.get());
		return this;
	}

	public FluentFormItem textArea(FluentTextArea text) {
		item.setWidget(text.get());
		return this;
	}

	public FluentFormItem textField(FluentTextField text) {
		item.setWidget(text.get());
		return this;
	}
	
	public FluentFormItem inject(FluentInject inject) {
		item.setWidget(inject.get());
		return this;
	}
	
	public FormItem get() {
		return item;
	}
}
