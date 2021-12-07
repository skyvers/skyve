package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.metadata.view.TextOutput.Sanitisation;

public class FluentBlurb extends FluentWidget implements FluentAbsoluteSize<FluentBlurb> {
	private Blurb blurb = null;

	public FluentBlurb() {
		blurb = new Blurb();
	}

	public FluentBlurb(Blurb blurb) {
		this.blurb = blurb;
	}

	public FluentBlurb from(@SuppressWarnings("hiding") Blurb blurb) {

		markup(blurb.getMarkup());

		absoluteSize(blurb, this);

		invisibleConditionName(blurb.getInvisibleConditionName());
		textAlignment(blurb.getTextAlignment());
		Boolean b = blurb.getEscape();
		if (b != null) {
			escape(b.booleanValue());
		}
		sanitise(blurb.getSanitise());

		blurb.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		return this;
	}

	public FluentBlurb putProperty(String k, String v) {
		blurb.getProperties().put(k, v);
		return this;
	}

	public FluentBlurb markup(String markup) {
		blurb.setMarkup(markup);
		return this;
	}

	@Override
	public FluentBlurb pixelWidth(int pixelWidth) {
		blurb.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	@Override
	public FluentBlurb pixelHeight(int pixelHeight) {
		blurb.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	public FluentBlurb invisibleConditionName(String invisibleConditionName) {
		blurb.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	public FluentBlurb textAlignment(HorizontalAlignment textAlignment) {
		blurb.setTextAlignment(textAlignment);
		return this;
	}

	public FluentBlurb escape(boolean escape) {
		blurb.setEscape(escape ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentBlurb sanitise(Sanitisation sanitise) {
		blurb.setSanitise(sanitise);
		return this;
	}

	@Override
	public Blurb get() {
		return blurb;
	}
}
