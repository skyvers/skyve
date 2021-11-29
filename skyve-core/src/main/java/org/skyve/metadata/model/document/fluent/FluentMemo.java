package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Memo;

public class FluentMemo extends FluentConstrainableField<FluentMemo> {
	private Memo memo = new Memo();
	
	public FluentMemo() {
		// nothing to see
	}
	
	public FluentMemo(Memo memo) {
		super(memo);
	}
	
	@Override
	public Memo get() {
		return memo;
	}
}
