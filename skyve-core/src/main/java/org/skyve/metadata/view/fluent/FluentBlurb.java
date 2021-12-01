package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Blurb;

public class FluentBlurb extends FluentWidget {
	private Blurb blurb = null;
		
	public FluentBlurb() {
		blurb = new Blurb();
	}
	
	public FluentBlurb(Blurb blurb) {
		this.blurb = blurb;
	}
	
	public FluentBlurb from(@SuppressWarnings("hiding") Blurb blurb) {
		return this;
	}
	
	@Override
	public Blurb get() {
		return blurb;
	}
}
