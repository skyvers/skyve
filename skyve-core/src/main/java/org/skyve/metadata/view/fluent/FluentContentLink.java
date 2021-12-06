package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;

public class FluentContentLink extends FluentInputWidget<FluentContentLink> implements FluentAbsoluteWidth<FluentContentLink> {
	private ContentLink link = null;

	public FluentContentLink() {
		link = new ContentLink();
	}

	public FluentContentLink(ContentLink link) {
		this.link = link;
	}

	public FluentContentLink from(@SuppressWarnings("hiding") ContentLink link) {

		value(link.getValue());
		Boolean b = link.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}

		absoluteWidth(link, this);

		link.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		super.from(link);
		return this;
	}

	public FluentContentLink value(String value) {
		link.setValue(value);
		return this;
	}

	public FluentContentLink editable(boolean editable) {
		link.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public FluentContentLink pixelWidth(int width) {
		link.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	public FluentContentLink addParameter(FluentParameter parameter) {
		link.getParameters().add(parameter.get());
		return this;
	}

	@Override
	public ContentLink get() {
		return link;
	}
}
