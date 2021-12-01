package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Memo;

public class FluentMemo extends FluentConstrainableField<FluentMemo> {
	private Memo memo = null;
	
	public FluentMemo() {
		memo = new Memo();
	}
	
	public FluentMemo(Memo memo) {
		this.memo = memo;
	}

	public FluentMemo from(@SuppressWarnings("hiding") Memo memo) {
		super.from(memo);
		return this;
	}
	
	@Override
	public Memo get() {
		return memo;
	}
}
