package org.skyve.impl.web.faces.pipeline.component;

public class MobileComponentBuilderChain extends ComponentBuilderChain {
	public MobileComponentBuilderChain() {
		super(new MobileComponentBuilder(), new PaginatedListGridBuilder());
	}
}
