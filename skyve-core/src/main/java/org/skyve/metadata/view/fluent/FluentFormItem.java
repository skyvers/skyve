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
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
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

import jakarta.annotation.Nonnull;

/**
 * Fluent builder for {@link FormItem} metadata.
 */
public class FluentFormItem {
	private FormItem item = null;
	
	/**
	 * Creates a builder backed by a new {@link FormItem} metadata instance.
	 */
	public FluentFormItem() {
		item = new FormItem();
	}

	/**
	 * Creates a builder backed by the supplied form-item metadata instance.
	 *
	 * @param item
	 *            the form-item metadata to mutate
	 */
	public FluentFormItem(FormItem item) {
		this.item = item;
	}

	/**
	 * Copies form-item settings and converts the item widget to its fluent equivalent.
	 *
	 * <p>Side effects: replaces span/alignment/label/help/required settings and swaps the current
	 * widget reference with the converted widget.
	 *
	 * @param item
	 *            the source form-item metadata
	 * @return this builder
	 * @throws IllegalStateException
	 *             if the widget type is not supported by this fluent adapter
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
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
		else if (widget instanceof ContentUpload content) {
			content(new FluentContentUpload().from(content));
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

	/**
	 * Sets the column span for this form item.
	 *
	 * @param colspan
	 *            number of columns this item spans
	 * @return this builder
	 */
	public FluentFormItem colspan(int colspan) {
		item.setColspan(Integer.valueOf(colspan));
		return this;
	}
	
	/**
	 * Sets the row span for this form item.
	 *
	 * @param rowspan
	 *            number of rows this item spans
	 * @return this builder
	 */
	public FluentFormItem rowspan(int rowspan) {
		item.setRowspan(Integer.valueOf(rowspan));
		return this;
	}

	/**
	 * Sets the horizontal alignment for this form item.
	 *
	 * @param horizontalAlignment
	 *            item horizontal alignment
	 * @return this builder
	 */
	public FluentFormItem horizontalAlignment(HorizontalAlignment horizontalAlignment) {
		item.setHorizontalAlignment(horizontalAlignment);
		return this;
	}

	/**
	 * Sets the label text for this form item.
	 *
	 * @param label
	 *            label text
	 * @return this builder
	 */
	public FluentFormItem label(String label) {
		item.setLabel(label);
		return this;
	}

	/**
	 * Controls whether the item label is rendered.
	 *
	 * @param showLabel
	 *            {@code true} to show the label
	 * @return this builder
	 */
	public FluentFormItem showLabel(boolean showLabel) {
		item.setShowLabel(showLabel ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets horizontal alignment for the item label.
	 *
	 * @param labelHorizontalAlignment
	 *            label horizontal alignment
	 * @return this builder
	 */
	public FluentFormItem labelHorizontalAlignment(HorizontalAlignment labelHorizontalAlignment) {
		item.setLabelHorizontalAlignment(labelHorizontalAlignment);
		return this;
	}

	/**
	 * Controls whether inline help is shown.
	 *
	 * @param showHelp
	 *            {@code true} to show help
	 * @return this builder
	 */
	public FluentFormItem showHelp(boolean showHelp) {
		item.setShowHelp(showHelp ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets help text for this item.
	 *
	 * @param help
	 *            help text
	 * @return this builder
	 */
	public FluentFormItem help(String help) {
		item.setHelp(help);
		return this;
	}
	
	/**
	 * Controls whether this item is required.
	 *
	 * @param required
	 *            {@code true} if value is required
	 * @return this builder
	 */
	public FluentFormItem required(boolean required) {
		item.setRequired(required ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Assigns the default widget implementation.
	 *
	 * @param widget
	 *            default widget builder
	 * @return this builder
	 */
	public FluentFormItem defaultWidget(FluentDefaultWidget widget) {
		item.setWidget(widget.get());
		return this;
	}
	
	/**
	 * Assigns a managed-content upload.
	 *
	 * @param content managed-content upload builder
	 * @return this builder
	 */
	public @Nonnull FluentFormItem content(@Nonnull FluentContentUpload content) {
		item.setWidget(content.get());
		return this;
	}

	/**
	 * Assigns a content-signature widget.
	 *
	 * @param signature
	 *            content-signature widget builder
	 * @return this builder
	 */
	public FluentFormItem contentSignature(FluentContentSignature signature) {
		item.setWidget(signature.get());
		return this;
	}
	
	/**
	 * Assigns an action button widget.
	 *
	 * @param button
	 *            button widget builder
	 * @return this builder
	 */
	public FluentFormItem button(FluentButton button) {
		item.setWidget(button.get());
		return this;
	}
	
	/**
	 * Assigns a zoom-in widget.
	 *
	 * @param zoomIn
	 *            zoom-in widget builder
	 * @return this builder
	 */
	public FluentFormItem zoomIn(FluentZoomIn zoomIn) {
		item.setWidget(zoomIn.get());
		return this;
	}
	
	/**
	 * Assigns a dialog button widget.
	 *
	 * @param button
	 *            dialog button widget builder
	 * @return this builder
	 */
	public FluentFormItem dialogButton(FluentDialogButton button) {
		item.setWidget(button.get());
		return this;
	}
	
	/**
	 * Assigns a geometry input widget.
	 *
	 * @param geometry
	 *            geometry widget builder
	 * @return this builder
	 */
	public FluentFormItem geometry(FluentGeometry geometry) {
		item.setWidget(geometry.get());
		return this;
	}

	/**
	 * Assigns a geometry-map widget.
	 *
	 * @param map
	 *            geometry-map widget builder
	 * @return this builder
	 */
	public FluentFormItem geometryMap(FluentGeometryMap map) {
		item.setWidget(map.get());
		return this;
	}
	
	/**
	 * Assigns an HTML widget.
	 *
	 * @param html
	 *            HTML widget builder
	 * @return this builder
	 */
	public FluentFormItem html(FluentHTML html) {
		item.setWidget(html.get());
		return this;
	}
	
	/**
	 * Assigns a label widget.
	 *
	 * @param label
	 *            label widget builder
	 * @return this builder
	 */
	public FluentFormItem label(FluentLabel label) {
		item.setWidget(label.get());
		return this;
	}
	
	/**
	 * Assigns a blurb widget.
	 *
	 * @param blurb
	 *            blurb widget builder
	 * @return this builder
	 */
	public FluentFormItem blurb(FluentBlurb blurb) {
		item.setWidget(blurb.get());
		return this;
	}
	
	/**
	 * Assigns a progress-bar widget.
	 *
	 * @param bar
	 *            progress-bar widget builder
	 * @return this builder
	 */
	public FluentFormItem progressBar(FluentProgressBar bar) {
		item.setWidget(bar.get());
		return this;
	}
	
	/**
	 * Assigns a check-box widget.
	 *
	 * @param check
	 *            check-box widget builder
	 * @return this builder
	 */
	public FluentFormItem checkBox(FluentCheckBox check) {
		item.setWidget(check.get());
		return this;
	}
	
	/**
	 * Assigns a colour-picker widget.
	 *
	 * @param colour
	 *            colour-picker widget builder
	 * @return this builder
	 */
	public FluentFormItem colourPicker(FluentColourPicker colour) {
		item.setWidget(colour.get());
		return this;
	}
	
	/**
	 * Assigns a combo widget.
	 *
	 * @param combo
	 *            combo widget builder
	 * @return this builder
	 */
	public FluentFormItem combo(FluentCombo combo) {
		item.setWidget(combo.get());
		return this;
	}
	
	/**
	 * Assigns a radio widget.
	 *
	 * @param radio
	 *            radio widget builder
	 * @return this builder
	 */
	public FluentFormItem radio(FluentRadio radio) {
		item.setWidget(radio.get());
		return this;
	}
	
	/**
	 * Assigns a lookup-description widget.
	 *
	 * @param lookup
	 *            lookup-description widget builder
	 * @return this builder
	 */
	public FluentFormItem lookupDescription(FluentLookupDescription lookup) {
		item.setWidget(lookup.get());
		return this;
	}
	
	/**
	 * Assigns a password widget.
	 *
	 * @param password
	 *            password widget builder
	 * @return this builder
	 */
	public FluentFormItem password(FluentPassword password) {
		item.setWidget(password.get());
		return this;
	}
	
	/**
	 * Assigns a rich-text widget.
	 *
	 * @param text
	 *            rich-text widget builder
	 * @return this builder
	 */
	public FluentFormItem richText(FluentRichText text) {
		item.setWidget(text.get());
		return this;
	}
	
	/**
	 * Assigns a slider widget.
	 *
	 * @param slider
	 *            slider widget builder
	 * @return this builder
	 */
	public FluentFormItem slider(FluentSlider slider) {
		item.setWidget(slider.get());
		return this;
	}
	
	/**
	 * Assigns a spacer widget.
	 *
	 * @param spacer
	 *            spacer widget builder
	 * @return this builder
	 */
	public FluentFormItem spacer(FluentSpacer spacer) {
		item.setWidget(spacer.get());
		return this;
	}
	
	/**
	 * Assigns a spinner widget.
	 *
	 * @param spinner
	 *            spinner widget builder
	 * @return this builder
	 */
	public FluentFormItem spinner(FluentSpinner spinner) {
		item.setWidget(spinner.get());
		return this;
	}
	
	/**
	 * Assigns a static-image widget.
	 *
	 * @param image
	 *            static-image widget builder
	 * @return this builder
	 */
	public FluentFormItem staticImage(FluentStaticImage image) {
		item.setWidget(image.get());
		return this;
	}

	/**
	 * Assigns a link widget.
	 *
	 * @param link
	 *            link widget builder
	 * @return this builder
	 */
	public FluentFormItem link(FluentLink link) {
		item.setWidget(link.get());
		return this;
	}

	/**
	 * Assigns a text-area widget.
	 *
	 * @param text
	 *            text-area widget builder
	 * @return this builder
	 */
	public FluentFormItem textArea(FluentTextArea text) {
		item.setWidget(text.get());
		return this;
	}

	/**
	 * Assigns a text-field widget.
	 *
	 * @param text
	 *            text-field widget builder
	 * @return this builder
	 */
	public FluentFormItem textField(FluentTextField text) {
		item.setWidget(text.get());
		return this;
	}
	
	/**
	 * Assigns an inject widget.
	 *
	 * @param inject
	 *            inject widget builder
	 * @return this builder
	 */
	public FluentFormItem inject(FluentInject inject) {
		item.setWidget(inject.get());
		return this;
	}
	
	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable form-item metadata
	 */
	public FormItem get() {
		return item;
	}
}
