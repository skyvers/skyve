package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.reference.Reference;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.widget.Link;

public class FluentLink extends FluentWidget implements FluentAbsoluteWidth<FluentLink> {
	private Link link = null;

	public FluentLink() {
		link = new Link();
	}

	public FluentLink(Link link) {
		this.link = link;
	}

	public FluentLink from(@SuppressWarnings("hiding") Link link) {

		reference(link.getReference());
		target(link.getTarget());
		value(link.getValue());
		invisibleConditionName(link.getInvisibleConditionName());

		absoluteWidth(link, this);

		return this;
	}

	public FluentLink reference(Reference reference) {
		link.setReference(reference);
		return this;
	}

	public FluentLink target(ReferenceTarget target) {
		link.setTarget(target);
		return this;
	}

	public FluentLink value(String value) {
		link.setValue(value);
		return this;
	}

	public FluentLink invisibleConditionName(String invisibleConditionName) {
		link.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public FluentLink pixelWidth(int width) {
		link.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public Link get() {
		return link;
	}
}
